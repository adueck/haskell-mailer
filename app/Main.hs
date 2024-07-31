{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad (forever)
import DB qualified
import Data.Text qualified as Text
import Database.PostgreSQL.Simple
import Handlers qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Network.WebSockets qualified as WS
import Web.Scotty as Sc (delete, get, middleware, post, scottyApp)

-- TODO: Caching of cabal

main :: IO ()
main = do
  conn <- DB.makeDBConnection
  let settings = Warp.setPort 8080 Warp.defaultSettings
  sapp <- webApp conn
  putStrLn "Server running on 8080"
  Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions wsapp sapp

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

webApp :: Connection -> IO Wai.Application
webApp conn = Sc.scottyApp $ do
  middleware simpleCors
  middleware $ staticPolicy (noDots >-> addBase "static")
  -- Client-facing HTTP Handlers
  --  (web app)
  get "/" (H.showHome conn)
  get "/contacts" (H.indexContacts conn "")
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
  post "/mailing/delete/:_id" (H.destroyMailing conn)
  get "/enough/:_id" (H.unsubscribePage conn)
  post "/enough/:_id" (H.handleUnsubscribe conn)
  get "/change/:_id" (H.showSelfUpdate conn)
  post "/change/:_id" H.handleSelfUpdate
