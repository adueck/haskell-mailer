{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad (forever)
import DB qualified
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Time.Clock
import Data.Vault.Lazy qualified as Vault
import Database.PostgreSQL.Simple
import EnvBuddy (getAppEnv)
import Handlers qualified as H
import Network.HTTP.Types (status302)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Network.Wai.Session (Session, withSession)
import Network.Wai.Session.Map (mapStore_)
import Network.WebSockets qualified as WS
import Types
import Web.Cookie
import Web.Scotty as Sc (delete, get, middleware, post, scottyApp)

-- TODO: Caching of cabal

main :: IO ()
main = do
  conn <- DB.makeDBConnection
  session <- Vault.newKey
  store <- mapStore_
  let settings = Warp.setPort 8080 Warp.defaultSettings
  sapp <- webApp session conn
  putStrLn "Server running on 8080"
  Warp.runSettings
    settings
    $ WaiWs.websocketsOr
      WS.defaultConnectionOptions
      wsapp
      ( withSession
          store
          (fromString "session")
          defaultSetCookie
          session
          sapp
      )

wsapp :: WS.ServerApp
wsapp pending = do
  conn <- WS.acceptRequest pending
  -- (msg :: Text.Text) <- WS.receiveData conn
  -- WS.sendTextData conn $ ("initial> " :: Text.Text) <> msg
  forever $ do
    WS.sendTextData conn ("loop data" :: Text.Text)
    threadDelay $ 1 * 1000000

-- putText "ws connected"
-- conn <- WS.acceptRequest pending
-- WS.forkPingThread conn 30

-- (msg :: Text) <- WS.receiveData conn
-- WS.sendTextData conn $ ("initial> " :: Text) <> msg

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

webApp :: Vault.Key (Session IO String String) -> Connection -> IO Wai.Application
webApp session conn = Sc.scottyApp $ do
  middleware simpleCors
  middleware $ staticPolicy (noDots >-> addBase "static")
  middleware $ withAuth session
  get "/login" (H.showLogin session)
  post "/login" (H.handleLogin session)
  post "/logout" (H.handleLogout session)
  -- Client-facing HTTP Handlers
  --  (web app)
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
