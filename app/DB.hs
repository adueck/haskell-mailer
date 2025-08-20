{-# LANGUAGE OverloadedStrings #-}

module DB
  ( getContacts,
    getMailings,
    getContactById,
    getMailingById,
    queryContacts,
    removeContact,
    removeMailing,
    addContact,
    addSend,
    addMailing,
    updateContact,
    updateNameEmailContact,
    updateMailing,
    markMailingPublished,
    makeDBConnection,
    getFirstMailing,
    getMailingSends,
    getContactSends,
    toContact,
  )
where

import Data.UUID.Types (UUID)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import EnvBuddy (getAppEnv)
import Types

toContact :: ContactOutput -> Contact
toContact (_id, name, email, group, notes) =
  Contact
    { contactId = _id,
      contactName = name,
      contactEmail = email,
      contactGroup = group,
      contactNotes = notes
    }

toMailing :: MailingOutput -> Mailing
toMailing (_id, subject, content, created, modified, published) =
  Mailing
    { mailingId = _id,
      mailingSubject = subject,
      mailingContent = content,
      mailingDate = created,
      mailingModified = modified,
      mailingPublished = published
    }

toSend :: SendOutput -> Send
toSend (send_id, send_mailing, send_contact, send_email, send_error, send_date) =
  Send
    { sendId = send_id,
      sendMailing = send_mailing,
      sendContact = send_contact,
      sendEmail = send_email,
      sendError = send_error,
      sendDate = send_date
    }

type MailingOutput = (UUID, String, String, ZonedTimestamp, ZonedTimestamp, Bool)

type ContactOutput = (UUID, String, String, String, String)

type SendOutput = (UUID, UUID, UUID, String, String, ZonedTimestamp)

getContacts :: Connection -> IO [Contact]
getContacts conn = do
  res <- query_ conn "SELECT * FROM contacts" :: IO [ContactOutput]
  return $ fmap toContact res

getMailings :: Connection -> IO [Mailing]
getMailings conn = do
  res <- query_ conn "SELECT * FROM mailings ORDER BY created_on DESC" :: IO [MailingOutput]
  return $ fmap toMailing res

getContactById :: Connection -> UUID -> IO (Maybe Contact)
getContactById conn _id = do
  x :: [ContactOutput] <-
    query
      conn
      "SELECT * FROM contacts WHERE contact_id = ? "
      (Only _id :: Only UUID)
  if null x
    then return Nothing
    else return $ Just (toContact $ head x)

getContactSends :: Connection -> UUID -> IO [(String, Send)]
getContactSends conn _id = do
  x :: [(String, UUID, UUID, UUID, String, String, ZonedTimestamp)] <-
    query
      conn
      "SELECT m.subject, s.send_id, s.mailing_id, s.contact_id, s.email, s.send_error, s.sent_on \
      \ FROM sends s \
      \ INNER JOIN mailings m ON s.mailing_id = m.mailing_id \
      \ WHERE contact_id = ?"
      (Only _id :: Only UUID)
  return $ map (\(subj, a, b, c, d, e, r) -> (subj, Send a b c d e r)) x

getMailingById :: Connection -> UUID -> IO (Maybe Mailing)
getMailingById conn _id = do
  x :: [MailingOutput] <-
    query
      conn
      "SELECT * FROM mailings WHERE mailing_id = ? "
      (Only _id :: Only UUID)
  if null x
    then return Nothing
    else return $ Just (toMailing $ head x)

getMailingSends :: Connection -> UUID -> IO [Send]
getMailingSends conn mailing_id = do
  x :: [SendOutput] <-
    query
      conn
      "SELECT DISTINCT ON (contact_id) * \
      \ FROM sends WHERE mailing_id = ? ORDER BY contact_id, (send_error != ''), sent_on DESC;"
      (Only mailing_id :: Only UUID)
  return $ toSend <$> x

getFirstMailing :: Connection -> IO (Maybe Mailing)
getFirstMailing conn = do
  x :: [MailingOutput] <-
    query
      conn
      "SELECT * FROM mailings LIMIT 1"
      ()
  if null x
    then return Nothing
    else return $ Just (toMailing $ head x)

queryContacts :: Connection -> String -> IO [Contact]
queryContacts conn qs = do
  let q = "%" ++ qs ++ "%"
  x :: [ContactOutput] <-
    query
      conn
      "SELECT * FROM contacts WHERE (name LIKE ? ) OR (email LIKE ? ) OR (contact_group LIKE ? ) or (notes LIKE ? )"
      (q, q, q, q)
  return $ fmap toContact x

removeContact :: Connection -> UUID -> IO (Either String ())
removeContact conn _id = do
  res <- getContactById conn _id
  case res of
    Nothing -> return (Left "Contact not found for deletion")
    Just _ -> do
      r <-
        execute
          conn
          "DELETE FROM contacts WHERE contact_id = ? "
          (Only _id :: Only UUID)
      return $
        if r == 1
          then Right ()
          else Left "Error deleting contact"

removeMailing :: Connection -> UUID -> IO (Either String ())
removeMailing conn _id = do
  res <- getMailingById conn _id
  case res of
    Nothing -> return (Left "Mailing not found for deletion")
    Just _ -> do
      r <-
        execute
          conn
          "DELETE FROM mailings WHERE mailing_id = ? "
          (Only _id :: Only UUID)
      return $
        if r == 1
          then Right ()
          else Left "Error deleting mailing"

addContact :: Connection -> Contact -> IO (Either String ())
addContact conn contact = do
  res <- queryContacts conn (contactEmail contact)
  case res of
    [] -> do
      r <- execute conn "INSERT INTO contacts VALUES (default, ?, ?, ?, ?)" (contactName contact, contactEmail contact, contactGroup contact, contactNotes contact)
      if r == 1
        then return $ Right ()
        else return $ Left "Error adding contact"
    _ -> return $ Left "E-mail contact already exists!"

addMailing :: Connection -> (String, String) -> IO (Either String ())
addMailing conn (subj, content) = do
  r <- execute conn "INSERT INTO mailings VALUES (default, ?, ?)" (subj, content)
  if r == 1
    then return $ Right ()
    else return $ Left "Error adding mailing"

addSend :: Connection -> UUID -> Contact -> String -> IO (Either String ())
addSend conn mailing_id contact err = do
  r <- execute conn "INSERT INTO sends VALUES (default, ?, ?, ?, ?)" (mailing_id, contactId contact, contactEmail contact, err)
  if r == 1
    then return $ Right ()
    else return $ Left "Error adding send"

updateContact :: Connection -> Contact -> IO (Either String ())
updateContact conn contact = do
  res <- getContactById conn (contactId contact)
  case res of
    Nothing -> return $ Left "Contact not found!"
    Just _ -> do
      r <-
        execute
          conn
          "UPDATE contacts SET name = ?, email = ?, contact_group = ?, notes = ? \
          \ WHERE contact_id = ?"
          (contactName contact, contactEmail contact, contactGroup contact, contactNotes contact, contactId contact)
      if r == 1
        then return $ Right ()
        else return $ Left "Error updating contact"

updateNameEmailContact :: Connection -> UUID -> String -> String -> IO (Either String ())
updateNameEmailContact conn _id name email = do
  res <- getContactById conn _id
  case res of
    Nothing -> return $ Left "Contact not found!"
    Just _ -> do
      r <-
        execute
          conn
          "UPDATE contacts SET name = ?, email = ? \
          \ WHERE contact_id = ?"
          (name, email, _id)
      if r == 1
        then return $ Right ()
        else return $ Left "Error updating contact"

markMailingPublished :: Connection -> UUID -> IO (Either String ())
markMailingPublished conn _id = do
  r <-
    execute
      conn
      "UPDATE mailings SET published = ? WHERE mailing_id = ?"
      (True, _id)
  if r == 1
    then return $ Right ()
    else return $ Left "Error marking mailing published"

updateMailing :: Connection -> (UUID, String, String) -> IO (Either String ())
updateMailing conn (_id, subject, content) = do
  res <- getMailingById conn _id
  case res of
    Nothing -> return $ Left "Mailing not found!"
    Just _ -> do
      r <-
        execute
          conn
          "UPDATE mailings SET subject = ?, content = ?, updated_on = CURRENT_TIMESTAMP \
          \ WHERE mailing_id = ?"
          (subject, content, _id)
      if r == 1
        then return $ Right ()
        else return $ Left "Error updating mailing"

localPG :: IO ConnectInfo
localPG = do
  env <- getAppEnv
  return
    defaultConnectInfo
      { connectDatabase = dbEnv env,
        connectHost = dbHostEnv env,
        connectPassword = dbPasswordEnv env
      }

makeDBConnection :: IO Connection
makeDBConnection = localPG >>= connect
