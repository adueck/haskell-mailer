{-# LANGUAGE OverloadedStrings #-}

module Main where

import DB qualified
import Data.String (fromString)
import Data.Vault.Lazy qualified as Vault
import Database.PostgreSQL.Simple
import EnvBuddy (getAppEnv)
import Handlers qualified as H
import Network.HTTP.Types (status302)
import Network.Wai qualified as Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Network.Wai.Session (Session, SessionStore, withSession)
import Network.Wai.Session.Map (mapStore_)
import Types
import Web.Cookie
import Web.Scotty

-- TODO: Caching of cabal

main :: IO ()
main = do
  conn <- DB.makeDBConnection
  session <- Vault.newKey
  store <- mapStore_
  webApp session store conn

withAuth :: Vault.Key (Session IO String String) -> Wai.Middleware
withAuth session app req respond = do
  -- no auth for routes where user changes their contact info
  if "change" `elem` Wai.pathInfo req || "enough" `elem` Wai.pathInfo req
    then do
      app req respond
    else do
      env <- getAppEnv
      let pswd = authPasswordEnv env
      if null pswd || (show (Wai.pathInfo req) == "[\"login\"]")
        then
          app req respond
        else case Vault.lookup session (Wai.vault req) of
          Just (sessionLookup, _) -> do
            u <- sessionLookup "u"
            if "logged in" `elem` u
              then app req respond
              else
                app req $ respond . Wai.mapResponseStatus (const status302) . Wai.mapResponseHeaders (\hs -> ("Location", "/login") : hs)
          Nothing -> app req respond

webApp :: Vault.Key (Session IO String String) -> SessionStore IO String String -> Connection -> IO ()
webApp session store conn = scotty 8080 $ do
  middleware simpleCors
  middleware $ staticPolicy (noDots >-> addBase "static")
  middleware $ withSession store (fromString "session") defaultSetCookie session
  middleware $ withAuth session
  get "/login" (H.showLogin session)
  post "/login" (H.handleLogin session)
  post "/logout" (H.handleLogout session)
  -- Client-facing HTTP Handlers
  --  (web app)
  -- TODO: Wrap these up into one ScottyM
  -- Then combine that with a reader so we can just pass the 
  -- conn as a reader env
  get "/" (H.showHome conn)
  get "/contacts" (H.indexContacts conn "")
  get "/upload-contacts" H.showUploadContacts
  post "/upload-contacts" (H.handleUploadContacts conn)
  get "/download-contacts" (H.handleDownloadContacts conn)
  get "/contact" H.showNewContact
  post "/contact" (H.createContact conn)
  get "/contact/:_id" (H.showContact conn)
  post "/contact/:_id" (H.updateContact conn)
  post "/contact/delete/:_id" (H.destroyContact conn)
  get "/mailing" H.showNewMailing
  get "/mailing/:_id" (H.showMailing conn)
  post "/mailing" (H.createMailing conn)
  post "/mailing/:_id" (H.updateMailing conn)
  post "/images/:mailing_id" H.handleUpload
  delete "/images/:mailing_id/:filename" H.handleUploadDelete
  -- post "/uploads/:filename" (H.deleteUpload)
  post "/send/mailing/:_id" (H.handleSendMailing conn)
  post "/send/mailing-oneoff/:_id" (H.handleSendOneOffMailing conn)
  post "/mailing/delete/:_id" (H.destroyMailing conn)
  get "/enough/:_id" (H.unsubscribePage conn)
  post "/enough/:_id" (H.handleUnsubscribe conn)
  get "/change/:_id" (H.showSelfUpdate conn)
  post "/change/:_id" (H.handleSelfUpdate conn)
