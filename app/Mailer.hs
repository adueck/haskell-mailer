{-# LANGUAGE OverloadedStrings #-}

module Mailer
  ( sendMailingP,
    sendAdminMail,
    sendOneOffMailing,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Exception
import DB qualified
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as DS
import Data.Text.Lazy qualified as DL
import Data.UUID (toString)
import Database.PostgreSQL.Simple
import EnvBuddy
import Network.Mail.Mime (Mail)
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP
import SimpleTemplate
import System.FilePath ((</>))
import Types
import Zenacy.HTML

contactToAddress :: Contact -> Address
contactToAddress contact = Address (Just (DS.pack (contactName contact))) (DS.pack (contactEmail contact))

sendAdminMail :: String -> String -> IO (Async ())
sendAdminMail subj body = do
  env <- getAppEnv
  send <- makeSender
  let mail = makeTextMail "HM Admin" (adminEmailEnv env) subj body
  async $ send mail

makeSender :: IO (Mail -> IO ())
makeSender = do
  env <- getAppEnv
  if null (domainEnv env)
    -- if no SMTP provided, use local mailpit for testing
    then return $ sendMail' "localhost" 1025
    else return $ sendMailWithLoginTLS' (domainEnv env) (read (portEnv env)) (loginEnv env) (passwordEnv env)

makeTextMail :: String -> String -> String -> String -> Mail
makeTextMail from to subject content =
  simpleMail
    (Address Nothing (DS.pack from))
    [Address Nothing (DS.pack to)]
    []
    []
    (DS.pack subject)
    [Mime.plainPart (DL.pack content)]

makeMail :: String -> String -> String -> Text -> Contact -> Mail
makeMail fromName fromEmail subject content to =
  simpleMail
    (Address (Just (DS.pack fromName)) (DS.pack fromEmail))
    [contactToAddress to]
    []
    []
    (DS.pack subject)
    [Mime.htmlPart $ DL.fromStrict content]

sendMailingP :: Connection -> Mailing -> [Contact] -> IO ()
sendMailingP dbConn (Mailing mailing_id subj content _ _ _) contacts = do
  env <- getAppEnv
  let contentU = changeImgTags (DS.pack content)
  send <- makeSender
  let makeM = makeMail (senderName env) (senderName env) subj
  putStrLn "GONING TO START MAILING"
  let tryOneMailing c = do
        putStrLn $ "Will send to " ++ contactEmail c
        -- ugly, clean up flow
        let makeM2 = do
              withTemplate <-
                renderTemplate
                  ("email-templates" </> "foundation.html")
                  [ ("BODY", DS.unpack contentU),
                    ("UNSUBSCRIBE_LINK", urlEnv env ++ "/enough/" ++ toString (contactId c)),
                    ("UPDATE_CONTACT_LINK", urlEnv env ++ "/change/" ++ toString (contactId c))
                  ]
              return $ makeM withTemplate c
        mail <- makeM2
        result <- try (send mail) :: IO (Either SomeException ())
        print result
        _ <-
          DB.addSend
            dbConn
            mailing_id
            c
            ( case result of
                Left err -> show err
                Right () -> ""
            )
        -- wait two seconds to ensure we don't send more than 30 e-mails per minute
        threadDelay 2000000
        return result
  mapM_ tryOneMailing contacts

sendOneOffMailing :: Mailing -> Contact -> IO ()
sendOneOffMailing (Mailing _ subj content _ _ _) contact = do
  env <- getAppEnv
  let contentU = changeImgTags (DS.pack content)
  send <- makeSender
  let makeM = makeMail (senderName env) (senderEmail env) subj
  -- ugly, clean up flow
  let makeM2 = do
        withTemplate <-
          renderTemplate
            ("email-templates" </> "foundation.html")
            [ ("BODY", DS.unpack contentU),
              ("UNSUBSCRIBE_LINK", urlEnv env ++ "/enough/d6803153-d078-43ff-bf1f-64a59cfe205d"),
              ("UPDATE_CONTACT_LINK", urlEnv env ++ "/change/d6803153-d078-43ff-bf1f-64a59cfe205d")
            ]
        return $ makeM withTemplate contact
  mail <- makeM2
  result <- try (send mail) :: IO (Either SomeException ())
  print result

changeImgTags :: Text -> Text
changeImgTags html = htmlRenderNodes $ getInsideBody $ htmlMapElem queryHtml (textToNode html)

getInsideBody :: HTMLNode -> [HTMLNode]
getInsideBody (HTMLElement _ _ _ bodyContent) = bodyContent
getInsideBody _ = []

textToNode :: Text -> HTMLNode
textToNode = fromJust . htmlSpaceRemove . fromJust . htmlDocBody . htmlParseEasy

queryHtml :: HTMLNode -> HTMLNode
queryHtml = htmlQueryTry $ do
  htmlQueryName "figure"
  htmlQueryFirst
  htmlQueryName "a"
  htmlQueryFirst
  htmlQueryName "img"
  img <- htmlQueryNode
  case htmlElemGetAttr "src" img of
    Just b ->
      htmlQuerySucc $
        htmlElem
          "img"
          [htmlAttr "src" b]
          [htmlText b]
    Nothing -> htmlQuerySucc img
