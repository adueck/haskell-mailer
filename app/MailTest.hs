{-# LANGUAGE OverloadedStrings #-}

import Network.Mail.SMTP

from = Address Nothing "email@domain.com"

to = [Address (Just "Jason Hickner") "email@domain.com"]

cc = []

bcc = []

subject = "email subject"

body = plainTextPart "email body"

html = htmlPart "<h1>HTML</h1>"

mail = simpleMail from to cc bcc subject [body, html]

main = do
  putStrLn "will send"
  sendMail' "localhost" 1025 mail
  putStrLn "sent"