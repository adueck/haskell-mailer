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
    handleSendMailing,
    handleUploadDelete,
    handleUpload,
    unsubscribePage,
    handleUnsubscribe,
    showSelfUpdate,
    handleSelfUpdate,
    handleSendM,
  )
where

import Control.Concurrent.Async (async)
import Control.Monad (when)
import Control.Monad.IO.Class
import DB qualified
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Text (unpack)
import Data.Text.Lazy qualified as T
import Data.UUID.Types
import Database.PostgreSQL.Simple
import EnvBuddy
import FileHelpers (getFileFromForm)
import Mailer as M
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

handleUploadDelete :: ActionM ()
handleUploadDelete = do
  mailing_id :: String <- pathParam "mailing_id"
  filename :: String <- pathParam "filename"
  let path = uploadBasePath </> mailing_id </> filename
  fileExists <- liftIO $ doesFileExist path
  liftIO $ when fileExists $ removeFile path
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
  _idS :: String <- pathParam "_id"
  case fromString _idS of
    Nothing -> redirect "/contacts"
    Just _id -> do
      contact <- liftIO $ DB.getContactById conn _id
      case contact of
        Nothing -> do
          redirect "/contacts"
        Just c -> do
          contactPage c

showMailing :: Connection -> ActionM ()
showMailing conn = do
  _idS :: String <- pathParam "_id"
  case fromString _idS of
    Nothing -> redirect "/mailings"
    Just _id -> do
      mailing <- liftIO $ DB.getMailingById conn _id
      case mailing of
        Nothing -> do
          redirect "/mailings"
        Just m -> do
          mailingPage m

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
  _idS :: String <- pathParam "_id"
  case fromString _idS of
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
  _idS :: String <- pathParam "_id"
  case fromString _idS of
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
  _idS :: String <- pathParam "_id"
  case fromString _idS of
    Nothing -> redirect "/contacts"
    Just _id -> do
      res <- liftIO $ DB.removeContact conn _id
      case res of
        Right _ -> redirect "/contacts"
        Left _ -> redirect "/contacts"

unsubscribePage :: Connection -> ActionM ()
unsubscribePage conn = do
  _idS :: String <- pathParam "_id"
  case fromString _idS of
    Nothing -> unsubscribeErrorPage
    Just _id -> do
      res <- liftIO $ DB.getContactById conn _id
      case res of
        Just contact -> unsubscribeConfirmationPage "" contact
        Nothing -> unsubscribeErrorPage

showSelfUpdate :: Connection -> ActionM ()
showSelfUpdate conn = do
  _idS :: String <- pathParam "_id"
  case fromString _idS of
    Nothing -> selfUpdateErrorPage
    Just _id -> do
      res <- liftIO $ DB.getContactById conn _id
      case res of
        Just contact -> selfUpdatePage "" contact
        Nothing -> selfUpdateErrorPage

handleUnsubscribe :: Connection -> ActionM ()
handleUnsubscribe conn = do
  ps <- formParams
  _idS :: String <- pathParam "_id"
  let checked = Just "on" == lookup "unsubscribe" ps
  case fromString _idS of
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

handleSelfUpdate :: ActionM ()
handleSelfUpdate = do
  json $ object ["ok" .= True]

-- ps <- formParams
-- _idS :: String <- pathParam "_id"
-- case fromString _idS of
-- (Just _id) -> do
--   json $ object ["ok" .= True]
-- _ -> json $ object ["ok" .= False]

destroyMailing :: Connection -> ActionM ()
destroyMailing conn = do
  _idS :: String <- pathParam "_id"
  case fromString _idS of
    Nothing -> redirect "/mailings"
    Just _id -> do
      res <- liftIO $ DB.removeMailing conn _id
      case res of
        Right _ -> do
          let path = uploadBasePath </> _idS
          directoryExist <- liftIO $ doesDirectoryExist path
          liftIO $ when directoryExist $ removeDirectoryRecursive path
          redirect "/"
        Left _ -> redirect "/"

-- Views

-- Helper Functions

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