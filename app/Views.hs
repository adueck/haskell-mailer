{-# LANGUAGE OverloadedStrings #-}

module Views
  ( contactsIndexPage,
    contactPage,
    mailingPage,
    newContactPage,
    newMailingPage,
    homePage,
    unsubscribeConfirmationPage,
    unsubscribeSuccessPage,
    unsubscribeErrorPage,
    selfUpdateErrorPage,
    selfUpdatePage,
    selfUpdateSuccessPage,
    uploadContactsPage,
    loginPage,
  )
where

import Control.Monad (unless)
import Data.UUID (toString)
import Data.UUID.Types (UUID, nil)
import Text.Blaze.Html (dataAttribute, stringValue)
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Types
import Web.Scotty

loginPage :: ActionM ()
loginPage =
  html $
    renderHtml $
      baseTemplate "Login" $ do
        H.div H.! A.class_ "container py-4" H.! A.style "max-width: 500px" $ do
          H.h1 "Login"
          H.form H.! A.method "POST" $ do
            H.div H.! A.class_ "my-4" $ do
              H.label "Password" H.! A.for "password" H.! A.class_ "form-label"
              H.input H.! A.name "password" H.! A.type_ "password" H.! A.class_ "form-control" H.! A.id "password"
              H.button "Submit" H.! A.class_ "btn btn-primary my-3" H.! A.type_ "submit"

homePage :: [Mailing] -> ActionM ()
homePage mailings = html $
  renderHtml $
    appTemplate "Mailings" "/" $ do
      H.ul H.! A.class_ "list-group list-group-flush" $ do
        if null mailings
          then H.p "No mailings yet!"
          else mapM_ mailingSummary mailings

contactsIndexPage :: String -> String -> [Contact] -> ActionM ()
contactsIndexPage msg search contacts = html $
  renderHtml $
    appTemplate "Contacts" "/contacts" $ do
      H.div H.! A.class_ "my-2 d-flex flex-row" $ do
        H.a H.! A.href "/contact" $ do
          H.button "Add Contact" H.! A.class_ "btn btn-primary me-4"
        H.a H.! A.href "/upload-contacts" $ do
          H.button "Upload CSV" H.! A.class_ "btn btn-secondary me-2"
        H.a H.! A.href "/download-contacts" $ do
          H.button "Download CSV" H.! A.class_ "btn btn-primary"
      searchBar search
      contactsTable contacts
      unless (search == "") $ H.a "Clear Search" H.! A.href "/contacts"
      H.p $ H.toHtml msg

contactPage :: Contact -> [(String, Send)] -> ActionM ()
contactPage contact sends = html $
  renderHtml $
    appTemplate "Edit Contact" "/contact" $ do
      contactForm contact ("/contact/" ++ show (contactId contact)) "Edit Contact"
      sendsInfo sends

uploadContactsPage :: ActionM ()
uploadContactsPage = html $
  renderHtml $
    appTemplate "Upload Contacts CSV" "/upload-contacts" $ do
      H.form H.! A.method "POST" H.! A.enctype "multipart/form-data" $ do
        H.div H.! A.class_ "mb-3" $ do
          H.label "Contacts CSV file for import" H.! A.for "formFile" H.! A.class_ "form-label"
          H.input H.! A.class_ "form-control" H.! A.type_ "file" H.! A.id "formFile" H.! A.name "file"
        H.button "Submit" H.! A.type_ "submit" H.! A.class_ "btn btn-primary"
      H.p "CSV must be formatted as follows"
      H.pre $ do
        H.code $ do
          "name,email,group,notes\nBob Smith,bob@example.com,,"

mailingPage :: Mailing -> [Send] -> ActionM ()
mailingPage mailing sends = html $
  renderHtml $
    appTemplate (mailingSubject mailing) "/mailing" $ do
      mailingForm
        (mailingId mailing)
        (mailingSubject mailing, mailingContent mailing, mailingPublished mailing)
        ("/mailing/" ++ show (mailingId mailing))
        "Save"
      H.h4 $ H.toHtml $ "Sent to " ++ show (length sends) ++ " contacts"
      sendsInfo (map ("",) sends)

newContactPage :: ActionM ()
newContactPage = html $
  renderHtml $
    appTemplate "New Contact" "/contact" $ do
      contactForm (Contact nil "" "" "" "") "/contact" "Create Contact"

newMailingPage :: ActionM ()
newMailingPage = html $
  renderHtml $
    appTemplate "New Mailing" "/mailing" $ do
      mailingForm nil ("", "", False) "/mailing" "Create Mailing"

unsubscribeSuccessPage :: Contact -> ActionM ()
unsubscribeSuccessPage c = html $
  renderHtml $
    baseTemplate "Unsubscribed!" $ do
      H.div H.! A.class_ "container py-4" $ do
        H.h2 "Unsubscribed" H.! A.class_ "mb-3"
        H.p $ do
          "Ok, you won't receive any more e-mails to "
          H.strong (H.toHtml $ contactEmail c)
          "."

unsubscribeErrorPage :: ActionM ()
unsubscribeErrorPage = html $
  renderHtml $
    baseTemplate "Error" $ do
      H.div H.! A.class_ "container py-4" $ do
        H.h2 "Error Unsubscribing" H.! A.class_ "mb-3"
        H.p $ do
          "Sorry! There was an error unsubscribing, or your address has already been removed."

selfUpdateErrorPage :: ActionM ()
selfUpdateErrorPage = html $
  renderHtml $
    baseTemplate "Error" $ do
      H.div H.! A.class_ "container py-4" H.! A.style "max-width: 600px;" $ do
        H.h2 "Error Updating Contact Info" H.! A.class_ "mb-3"
        H.p $ do
          "Sorry! There was an error in updating your contact info."

selfUpdateSuccessPage :: Contact -> ActionM ()
selfUpdateSuccessPage contact = html $
  renderHtml $
    baseTemplate "Contact Info Updated" $ do
      H.div H.! A.class_ "container py-4" H.! A.style "max-width: 600px;" $ do
        H.h2 "Your contact info has been updated" H.! A.class_ "mb-3"
        H.div H.! A.class_ "my-4" $
          H.toHtml $
            "Name: " ++ contactName contact
        H.div H.! A.class_ "my-4" $
          H.toHtml $
            "Email: " ++ contactEmail contact
        H.a H.! A.href (stringValue ("/change/" ++ toString (contactId contact))) $
          "Made a mistake?"

unsubscribeConfirmationPage :: String -> Contact -> ActionM ()
unsubscribeConfirmationPage errMsg c = html $
  renderHtml $
    baseTemplate "Unsubscribe" $ do
      H.div H.! A.class_ "container py-4" H.! A.style "max-width: 600px;" $ do
        H.h2 "Unsubscribe" H.! A.class_ "mb-3"
        H.p $ do
          "Want to stop receiving e-mails to "
          H.strong (H.toHtml $ contactEmail c)
          "?"
        H.form H.! A.method "POST" $ do
          H.div H.! A.class_ "form-check mb-4" $ do
            H.input H.! A.class_ "form-check-input" H.! A.type_ "checkbox" H.! A.name "unsubscribe" H.! A.id "unsubscribe-box"
            H.label
              "Yes, please take me off the update list."
              H.! A.class_ "form-check-label"
              H.! A.for "flexCheckDefault"
          H.button "Unsubscribe" H.! A.type_ "submit" H.! A.class_ "btn btn-primary"
        unless (null errMsg) $ do
          H.div H.! A.class_ "alert alert-warning" H.! A.role "alert" $
            H.toHtml errMsg

selfUpdatePage :: String -> Contact -> ActionM ()
selfUpdatePage errMsg c = html $
  renderHtml $
    baseTemplate "Update Contact Info" $ do
      H.div H.! A.class_ "container py-4" H.! A.style "max-width: 600px;" $ do
        H.h2 "Update Contact Info" H.! A.class_ "mb-3"
        H.p "Enter your new contact info here:"
        H.form H.! A.method "POST" $ do
          H.label "Name" H.! A.for "name-input" H.! A.class_ "form-label"
          H.input H.! A.name "name" H.! A.value (stringValue (contactName c)) H.! A.type_ "text" H.! A.id "name-input" H.! A.class_ "form-control mb-2"
          H.label "E-Mail" H.! A.for "name-input" H.! A.class_ "form-label"
          H.input H.! A.name "email" H.! A.value (stringValue (contactEmail c)) H.! A.type_ "email" H.! A.required "true" H.! A.id "name-input" H.! A.class_ "form-control mb-4"
          H.button "Update" H.! A.type_ "submit" H.! A.class_ "btn btn-primary"
        unless (null errMsg) $ do
          H.div H.! A.class_ "alert alert-warning" H.! A.role "alert" $
            H.toHtml errMsg

-- HTML Components

baseTemplate :: String -> H.Html -> H.Html
baseTemplate title children = H.html $ do
  H.head $ do
    H.title (H.toHtml title)
    H.link H.! A.href "/css/bootstrap.min.css" H.! A.rel "stylesheet"
    H.link H.! A.type_ "text/css" H.! A.href "/css/trix-2.css" H.! A.rel "stylesheet"
    H.script "" H.! A.type_ "text/javascript" H.! A.src "/js/trix-2.js"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
  H.body children

appTemplate :: String -> String -> H.Html -> H.Html
appTemplate title path children =
  baseTemplate title $ do
    H.nav H.! A.class_ "navbar navbar-expand-lg bg-light" $ do
      H.div H.! A.class_ "container-fluid" $ do
        H.a "MyMailer" H.! A.class_ "navbar-brand" H.! A.href "/"
        H.button
          H.! dataAttribute "bs-toggle" "collapse"
          H.! dataAttribute "bs-target" "#navbarSupportedContent"
          H.! A.class_ "navbar-toggler"
          H.! A.type_ "button"
          $ do
            H.span "" H.! A.class_ "navbar-toggler-icon"
        H.div H.! A.class_ "collapse navbar-collapse" H.! A.id "navbarSupportedContent" $ do
          H.ul H.! A.class_ "navbar-nav me-auto" $ do
            mapM_ (navLink path) [("Contacts", "/contacts"), ("New Mailing", "/mailing")]
          H.form H.! A.method "POST" H.! A.action "/logout" H.! A.class_ "form-inline my-2" $ do
            H.a H.! A.href "/user" $ do
              H.button "Logout" H.! A.class_ "btn btn-secondary btn-sm my-2 my-sm-0" H.! A.type_ "submit"
    H.div H.! A.class_ "container py-4" $ do
      H.h2 (H.toHtml title) H.! A.class_ "mb-4"
      children
    H.script "" H.! A.src "/js/bootstrap.bundle.min.js"

navLink :: String -> (String, String) -> H.Html
navLink path (label, url) = H.li H.! A.class_ "nav-item" $ do
  H.a (H.toHtml label)
    H.! A.class_ (stringValue $ (if path == url then "active " else "") ++ "nav-link")
    H.! A.href (stringValue url)

mailingSummary :: Mailing -> H.Html
mailingSummary mailing = do
  H.li H.! A.class_ "list-group-item" $ do
    H.div $ do
      H.a H.! A.href (stringValue ("/mailing/" ++ show (mailingId mailing))) $ do
        H.h5 $ do
          H.toHtml (mailingSubject mailing)
          H.span (if mailingPublished mailing then "Sent" else "Draft") H.! A.class_ (stringValue ("ms-4 badge text-bg-" ++ (if mailingPublished mailing then "success" else "secondary")))
    H.div $ H.toHtml $ "Updated on: " ++ show (mailingModified mailing)

mailingForm :: UUID -> (String, String, Bool) -> String -> String -> H.Html
mailingForm _id (subj, content, published) actionPath buttonText = do
  H.form H.! A.id "mailing-form" H.! A.onsubmit "handleMSubmit(event)" H.! A.method "POST" H.! A.class_ "row g-3" H.! A.style "max-width: 700px" $ do
    H.input H.! A.id "mailing-content" H.! A.type_ "hidden" H.! A.name "content" H.! A.value (stringValue content)
    H.div H.! A.id "subject-input" H.! A.class_ "col-md-12" $ do
      H.label "Subject:" H.! A.for "subject"
      H.input H.! A.class_ "form-control" H.! A.type_ "text" H.! A.name "subject" H.! A.value (stringValue subj)
    H.div "" H.! A.id "trix-wrapper" H.! A.class_ "col-md-12"
    H.div H.! A.class_ "d-flex flex-row justify-content-between" $ do
      H.button (H.toHtml buttonText) H.! A.type_ "submit" H.! A.formaction (stringValue actionPath) H.! A.class_ "btn btn-secondary"
      unless (buttonText == "Create Mailing") $ do
        H.button (H.toHtml ((if published then "Re-" else "") ++ "Send Mailing")) H.! A.type_ "submit" H.! A.formaction (stringValue ("/send/mailing/" ++ show _id)) H.! A.class_ "btn btn-success"
    H.script "" H.! A.src "/js/add-trix.js"
  unless (buttonText == "Create Mailing") $ do
    H.form H.! A.onsubmit "return confirm('Delete mailing?');" H.! A.class_ "my-4" H.! A.method "POST" H.! A.action (stringValue ("/mailing/delete/" ++ show _id)) $ do
      H.button "Delete" H.! A.type_ "submit" H.! A.class_ "btn btn-danger btn-sm"

contactForm :: Contact -> String -> String -> H.Html
contactForm contact actionPath buttonText = do
  H.div H.! A.style "max-width: 800px;" $ do
    H.form H.! A.class_ "row g-3" H.! A.method "post" $ do
      H.div H.! A.class_ "col-md-6" $ do
        H.label "Name:" H.! A.for "name"
        H.input H.! A.class_ "form-control" H.! A.type_ "text" H.! A.name "name" H.! A.value (stringValue (contactName contact))
      H.div H.! A.class_ "col-md-6" $ do
        H.label "E-mail:" H.! A.for "email"
        H.input H.! A.class_ "form-control" H.! A.type_ "e-mail" H.! A.name "email" H.! A.value (stringValue (contactEmail contact))
      H.div H.! A.class_ "col-md-6" $ do
        H.label "Group:" H.! A.for "group"
        H.input H.! A.class_ "form-control" H.! A.type_ "text" H.! A.name "group" H.! A.value (stringValue (contactGroup contact))
      H.div H.! A.class_ "col-md-6" $ do
        H.label "Notes:" H.! A.for "notes"
        H.input H.! A.class_ "form-control" H.! A.type_ "text" H.! A.name "notes" H.! A.value (stringValue (contactNotes contact))
      H.div H.! A.class_ "d-flex flex-row justify-content-between mt-4" $ do
        H.button (H.toHtml buttonText) H.! A.type_ "submit" H.! A.formaction (stringValue actionPath) H.! A.class_ "btn btn-secondary"
    H.form H.! A.class_ "d-flex flex-row-reverse" H.! A.onsubmit "return confirm('Delete contact?');" H.! A.method "POST" H.! A.action (stringValue ("/contact/delete/" ++ show (contactId contact))) $ do
      H.button "Delete Contact" H.! A.type_ "submit" H.! A.formaction (stringValue ("/contact/delete/" ++ show (contactId contact))) H.! A.class_ "btn btn-danger"

searchBar :: String -> H.Html
searchBar value = H.form H.! A.class_ "form-inline float-right mr-2 ml-3" H.! A.method "GET" $ do
  H.div H.! A.class_ "input-group mb-3" $ do
    H.input H.! A.class_ "form-control" H.! A.name "search" H.! A.type_ "text" H.! A.autocomplete "off" H.! A.placeholder "Search" H.! A.value (stringValue value)
    H.div H.! A.class_ "input-group-append" $ do
      H.button "Search" H.! A.class_ "input-group-text" H.! A.class_ "submit"

sendsInfo :: [(String, Send)] -> H.Html
sendsInfo sends = do
  unless (null sends) $ do
    let successes = filter (null . sendError . snd) sends
    let failures = filter (not . null . sendError . snd) sends
    H.div H.! A.style "max-width: 700px" $ do
      unless (null failures) $ do
        H.h5 $ H.toHtml $ "Failures (" ++ show (length failures) ++ ")"
        H.ul H.! A.class_ "list-group" $
          mapM_
            ( \(title, s) ->
                H.li H.! A.class_ "list-group-item list-group-item-danger" $ do
                  H.div (H.toHtml title)
                  H.div (H.toHtml $ sendEmail s ++ " " ++ sendError s)
                  H.div (H.toHtml (show (sendDate s)))
            )
            failures
      unless (null successes) $ do
        H.h5 $ H.toHtml $ "Successes (" ++ show (length successes) ++ ")"
        H.ul H.! A.class_ "list-group" $
          mapM_
            ( \(title, s) ->
                H.li H.! A.class_ "list-group-item" $ do
                  H.div (H.toHtml title)
                  H.div (H.toHtml $ sendEmail s ++ " " ++ show (sendDate s))
            )
            successes

contactsTable :: [Contact] -> H.Html
contactsTable contacts = do
  H.div (H.toHtml (show (length contacts) ++ " contacts"))
  H.table H.! A.class_ "table" $ do
    H.tr $ do
      H.th "Name" H.! A.scope "col"
      H.th "Email" H.! A.scope "col"
      H.th "Group" H.! A.scope "col"
      H.th "Notes" H.! A.scope "col"
    mapM_
      ( \(Contact _id contactName contactEmail contactGroup contactNotes) ->
          H.tr $ do
            H.td $ do
              H.a H.! A.href (stringValue ("/contact/" ++ show _id)) $ do
                H.toHtml contactName
            H.td $ H.toHtml contactEmail
            H.td $ H.toHtml contactGroup
            H.td $ H.toHtml contactNotes
      )
      contacts