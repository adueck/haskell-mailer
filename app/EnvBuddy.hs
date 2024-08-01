module EnvBuddy
  ( getAppEnv,
  )
where

import Data.Maybe
import System.Environment
import Types

getAppEnv :: IO AppEnv
getAppEnv = do
  urlR <- lookupEnv "MYMAILER_URL"
  senderEmailR <- lookupEnv "MYMAILER_SENDER"
  adminEmailR <- lookupEnv "MYMAILER_ADMIN_EMAIL"
  domainR <- lookupEnv "MYMAILER_DOMAIN"
  portR <- lookupEnv "MYMAILER_PORT"
  loginNameR <- lookupEnv "MYMAILER_LOGIN"
  passwordR <- lookupEnv "MYMAILER_PASSWORD"
  dbR <- lookupEnv "MYMAILER_DB"
  dbHostR <- lookupEnv "MYMAILER_DB_HOST"
  dbPasswordR <- lookupEnv "MYMAILER_DB_PASSWORD"
  return
    AppEnv
      { urlEnv = fromMaybe "http://localhost:8080" urlR,
        senderEnv = fromMaybe "sender@example.com" senderEmailR,
        adminEmailEnv = fromMaybe "admin@example.com" adminEmailR,
        domainEnv = fromMaybe "" domainR,
        portEnv = fromMaybe "465" portR,
        loginEnv = fromMaybe "" loginNameR,
        passwordEnv = fromMaybe "" passwordR,
        dbEnv = fromMaybe "postgres" dbR,
        dbHostEnv = fromMaybe "localhost" dbHostR,
        dbPasswordEnv = fromMaybe "postgres" dbPasswordR
      }