{-# LANGUAGE OverloadedStrings #-}

module Handlers
  ( indexContacts,
    showContact,
    showMailing,
    showNewContact,
    showNewMailing,
    updateContact,
    updateMailing,
    destroyContact,
    destroyMailing,
    createContact,
    createMailing,
    showHome,
    showLogin,
    handleSendMailing,
    handleUploadDelete,
    handleUpload,
    unsubscribePage,
    handleUnsubscribe,
    showSelfUpdate,
    handleSelfUpdate,
    handleSendM,
    showUploadContacts,
    handleUploadContacts,
    handleDownloadContacts,
    handleLogin,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad (when)
import Control.Monad.IO.Class
import DB qualified
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Csv qualified as CSV
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Text.Lazy qualified as T
import Data.UUID.Types
import Data.Vault.Lazy qualified as Vault
import Data.Vector (Vector)
import Database.PostgreSQL.Simple
import EnvBuddy
import FileHelpers (getFileFromForm)
import Mailer as M
import Network.Wai qualified as Wai
import Network.Wai.Session (Session, withSession)
import System.Directory
import System.FilePath ((</>))
import Types
import Views
import Web.Scotty

uploadBasePath :: FilePath
uploadBasePath = "static" </> "images"

handleUpload :: ActionM ()
handleUpload = do
  mailing_id :: String <- pathParam "mailing_id"
  env <- liftIO getAppEnv
  fRes <- getFileFromForm
  case fRes of
    Nothing -> json $ object ["error" .= ("error uploading file" :: String)]
    Just (fName, fContent) -> do
      liftIO $ createDirectoryIfMissing True (uploadBasePath </> mailing_id)
      liftIO $ B.writeFile (uploadBasePath </> mailing_id </> fName) fContent
      json $ object ["url" .= (urlEnv env ++ "/images/" ++ mailing_id ++ "/" ++ fName)]

handleUploadContacts :: Connection -> ActionM ()
handleUploadContacts conn = do
  fRes <- getFileFromForm
  case fRes of
    Nothing -> json $ object ["error" .= ("error uploading file" :: String)]
    Just (_, fContent) -> do
      let res = CSV.decode CSV.HasHeader fContent :: Either String (Vector (Text, Text, Text, Text))
      case res of
        Left _ -> redirect "/upload-contacts"
        Right contacts -> do
          _ <- liftIO $ mapM_ (DB.addContact conn) (fmap toContact contacts)
          redirect "/contacts"

handleDownloadContacts :: Connection -> ActionM ()
handleDownloadContacts conn = do
  contacts <- liftIO $ DB.getContacts conn
  setHeader "Content-Type" "text/csv"
  setHeader "Content-Disposition" "attachment; filename=contacts.csv"
  raw $ CSV.encode $ ("name", "email", "group", "notes") : (contactToWritable <$> contacts)

contactToWritable :: Contact -> (String, String, String, String)
contactToWritable (Contact _ name email group notes) =
  (name, email, group, notes)

toContact :: (Text, Text, Text, Text) -> Contact
toContact (name, email, group, notes) =
  Contact
    { contactId = nil,
      contactName = unpack name,
      contactEmail = unpack email,
      contactGroup = unpack group,
      contactNotes = unpack notes
    }

handleUploadDelete :: ActionM ()
handleUploadDelete = do
  mailing_id :: String <- pathParam "mailing_id"
  filename :: String <- pathParam "filename"
  let path = uploadBasePath </> mailing_id </> filename
  fileExists <- liftIO $ doesFileExist path
  liftIO $ when fileExists $ removeFile path
  json $ object ["ok" .= True]

showLogin :: ActionM ()
showLogin = do
  env <- liftIO getAppEnv
  if authPasswordEnv env == ""
    then
      redirect "/"
    else loginPage

handleLogin :: Vault.Key (Session IO String String) -> ActionM ()
handleLogin session = do
  env <- liftIO getAppEnv
  ps <- formParams
  req <- request
  let pEnv = authPasswordEnv env
  let password = unpack $ fromMaybe "" $ lookup "password" ps
  _ <- case Vault.lookup session (Wai.vault req) of
    Just (_, sessionInsert) -> do
      if password == pEnv
        then do
          liftIO $ sessionInsert "u" "logged in"
          redirect "/"
        else do
          liftIO $ threadDelay $ 1 * 2000000
          redirect "/login"
    Nothing -> redirect "/login"
  json $ object ["ok" .= True]

showHome :: Connection -> ActionM ()
showHome conn = do
  mailings <- liftIO $ DB.getMailings conn
  homePage mailings

indexContacts :: Connection -> String -> ActionM ()
indexContacts conn msg = do
  search :: Maybe String <- queryParamMaybe "search"
  case search of
    Just s -> do
      contacts <- liftIO $ DB.queryContacts conn s
      contactsIndexPage msg s contacts
    Nothing -> do
      contacts <- liftIO $ DB.getContacts conn
      contactsIndexPage msg "" contacts

createContact :: Connection -> ActionM ()
createContact conn = do
  contact <- contactFromParams nil <$> formParams
  case contact of
    Nothing -> indexContacts conn "invalid submission"
    Just b -> do
      res <- liftIO $ DB.addContact conn b
      case res of
        Left err -> indexContacts conn err
        Right _ -> redirect "/contacts"

createMailing :: Connection -> ActionM ()
createMailing conn = do
  mailing <- mailingFromParams nil <$> formParams
  case mailing of
    Nothing -> redirect "/"
    Just d -> do
      res <- liftIO $ DB.addMailing conn d
      case res of
        Left err -> indexContacts conn err
        Right _ -> redirect "/"

showNewContact :: ActionM ()
showNewContact = newContactPage

showNewMailing :: ActionM ()
showNewMailing = newMailingPage

showContact :: Connection -> ActionM ()
showContact conn = do
  pathId <- getPathId
  case pathId of
    Nothing -> redirect "/contacts"
    Just _id -> do
      contact <- liftIO $ DB.getContactById conn _id
      sends <- liftIO $ DB.getContactSends conn _id
      case contact of
        Nothing -> do
          redirect "/contacts"
        Just c -> do
          contactPage c sends

showMailing :: Connection -> ActionM ()
showMailing conn = do
  _idS :: String <- pathParam "_id"
  case fromString _idS of
    Nothing -> redirect "/mailings"
    Just _id -> do
      mailing <- liftIO $ DB.getMailingById conn _id
      sends <- liftIO $ DB.getMailingSends conn _id
      case mailing of
        Nothing -> do
          redirect "/mailings"
        Just m -> do
          mailingPage m sends

handleSendM :: Connection -> ActionM ()
handleSendM conn = do
  mailing <- liftIO $ DB.getFirstMailing conn
  contacts <- liftIO $ DB.getContacts conn
  case mailing of
    Just m -> do
      res <- liftIO $ M.sendMailingP conn m contacts
      json $ object ["res" .= show res]
    Nothing -> do
      json $ object ["res" .= False]

handleSendMailing :: Connection -> ActionM ()
handleSendMailing conn = do
  _id <- updateMailingStep conn
  res <- liftIO $ DB.markMailingPublished conn _id
  case res of
    Left _ -> redirect "/"
    Right _ -> do
      mailing <- liftIO $ DB.getMailingById conn _id
      contacts <- liftIO $ DB.getContacts conn
      case mailing of
        Nothing -> do
          redirect "/"
        Just d -> do
          _ <- liftIO $ async $ M.sendMailingP conn d contacts
          redirect "/"

updateContact :: Connection -> ActionM ()
updateContact conn = do
  ps <- formParams
  pathId <- getPathId
  case pathId of
    Nothing -> redirect "/contacts"
    Just _id -> do
      let contact = contactFromParams _id ps
      case contact of
        Nothing -> redirect "/"
        Just c -> do
          res <- liftIO $ DB.updateContact conn c
          case res of
            Left _ -> redirect "/contacts"
            Right _ -> redirect "/contacts"

updateMailing :: Connection -> ActionM ()
updateMailing conn = do
  _id <- updateMailingStep conn
  redirect (T.pack ("/mailing/" ++ toString _id))

updateMailingStep :: Connection -> ActionM UUID
updateMailingStep conn = do
  ps <- formParams
  pathId <- getPathId
  case pathId of
    Nothing -> redirect "/mailings"
    Just _id -> do
      let mailing = mailingFromParams _id ps
      case mailing of
        Nothing -> redirect "/"
        Just (subj, cont) -> do
          _ <- liftIO $ DB.updateMailing conn (_id, subj, cont)
          return _id

destroyContact :: Connection -> ActionM ()
destroyContact conn = do
  pathId <- getPathId
  case pathId of
    Nothing -> redirect "/contacts"
    Just _id -> do
      res <- liftIO $ DB.removeContact conn _id
      case res of
        Right _ -> redirect "/contacts"
        Left _ -> redirect "/contacts"

unsubscribePage :: Connection -> ActionM ()
unsubscribePage conn = do
  pathId <- getPathId
  case pathId of
    Nothing -> unsubscribeErrorPage
    Just _id -> do
      res <- liftIO $ DB.getContactById conn _id
      case res of
        Just contact -> unsubscribeConfirmationPage "" contact
        Nothing -> unsubscribeErrorPage

showSelfUpdate :: Connection -> ActionM ()
showSelfUpdate conn = do
  pathId <- getPathId
  case pathId of
    Nothing -> selfUpdateErrorPage
    Just _id -> do
      res <- liftIO $ DB.getContactById conn _id
      case res of
        Just contact -> selfUpdatePage "" contact
        Nothing -> selfUpdateErrorPage

showUploadContacts :: ActionM ()
showUploadContacts = uploadContactsPage

handleUnsubscribe :: Connection -> ActionM ()
handleUnsubscribe conn = do
  ps <- formParams
  pathId <- getPathId
  let checked = Just "on" == lookup "unsubscribe" ps
  case pathId of
    (Just _id) -> do
      c <- liftIO $ DB.getContactById conn _id
      case c of
        Just contact -> do
          if checked
            then do
              _ <- liftIO $ DB.removeContact conn _id
              _ <-
                liftIO $
                  sendAdminMail
                    "Unsubscription Notice"
                    (contactName contact ++ " (" ++ contactEmail contact ++ ") has unsubscribed")
              unsubscribeSuccessPage contact
            else
              unsubscribeConfirmationPage "Check the box to unsubscribe" contact
        _ -> unsubscribeErrorPage
    _ -> unsubscribeErrorPage

handleSelfUpdate :: Connection -> ActionM ()
handleSelfUpdate conn = do
  ps <- formParams
  pathId <- getPathId
  case pathId of
    Nothing -> selfUpdateErrorPage
    Just _id -> do
      let nameR = unpack <$> lookup "name" ps
      let emailR = unpack <$> lookup "email" ps
      case (nameR, emailR) of
        (Just name, Just email) -> do
          oldC <- liftIO $ DB.getContactById conn _id
          _ <- liftIO $ DB.updateNameEmailContact conn _id name email
          newC <- liftIO $ DB.getContactById conn _id
          case (oldC, newC) of
            (Just oldContact, Just newContact) -> do
              _ <-
                liftIO $
                  sendAdminMail
                    "Contact Updated"
                    ( "Old contact info: "
                        ++ contactName oldContact
                        ++ " "
                        ++ contactEmail oldContact
                        ++ "\nNew contact info: "
                        ++ contactName newContact
                        ++ " "
                        ++ contactEmail newContact
                    )
              selfUpdateSuccessPage newContact
            _ -> selfUpdateErrorPage
        _ -> selfUpdateErrorPage

destroyMailing :: Connection -> ActionM ()
destroyMailing conn = do
  pathId <- getPathId
  case pathId of
    Nothing -> redirect "/mailings"
    Just _id -> do
      res <- liftIO $ DB.removeMailing conn _id
      case res of
        Right _ -> do
          let path = uploadBasePath </> toString _id
          directoryExist <- liftIO $ doesDirectoryExist path
          liftIO $ when directoryExist $ removeDirectoryRecursive path
          redirect "/"
        Left _ -> redirect "/"

-- Views

-- Helper Functions

getPathId :: ActionM (Maybe UUID)
getPathId = do
  _idS :: String <- pathParam "_id"
  return $ fromString _idS

contactFromParams :: UUID -> [Param] -> Maybe Contact
contactFromParams _id p = do
  name <- unpack <$> lookup "name" p
  email <- unpack <$> lookup "email" p
  group <- unpack <$> lookup "group" p
  notes <- unpack <$> lookup "notes" p
  if name == "" || email == ""
    then Nothing
    else
      Just
        Contact
          { contactId = _id,
            contactName = name,
            contactEmail = email,
            contactGroup = group,
            contactNotes = notes
          }

mailingFromParams :: UUID -> [Param] -> Maybe (String, String)
mailingFromParams _id p = do
  subject <- unpack <$> lookup "subject" p
  content <- unpack <$> lookup "content" p
  if subject == ""
    then Nothing
    else
      Just (subject, content)