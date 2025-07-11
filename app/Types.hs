module Types
  ( Contact (..),
    Mailing (..),
    Send (..),
    AppEnv (..),
  )
where

import Data.UUID.Types
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)

data AppEnv
  = AppEnv
  { urlEnv :: String,
    senderEmail :: String,
    senderName :: String,
    adminEmailEnv :: String,
    domainEnv :: String,
    portEnv :: String,
    loginEnv :: String,
    passwordEnv :: String,
    dbEnv :: String,
    dbHostEnv :: String,
    dbPasswordEnv :: String,
    authPasswordEnv :: String
  }
  deriving (Show, Eq)

data Mailing = Mailing
  { mailingId :: UUID,
    mailingSubject :: String,
    mailingContent :: String,
    mailingDate :: ZonedTimestamp,
    mailingModified :: ZonedTimestamp,
    mailingPublished :: Bool
  }
  deriving (Show)

-- | Holds the information for one send of a mailing to one contact
data Send = Send
  { sendId :: UUID,
    -- | The MailingId of the mailing that was sent out
    sendMailing :: UUID,
    -- | The ContactId that received the send
    sendContact :: UUID,
    -- | The email that was used for the Contact receiving the mailing
    sendEmail :: String,
    -- | An error string if there was a problem in sending, null if everything
    --  was ok
    sendError :: String,
    -- | The date/time that the mailing was sent to this contact
    sendDate :: ZonedTimestamp
  }

data Contact = Contact
  { contactId :: UUID,
    contactName :: String,
    contactEmail :: String,
    contactGroup :: String,
    contactNotes :: String
  }
  deriving (Show, Eq)
