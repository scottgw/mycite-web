{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Home where

import Control.Monad

import Yesod.Auth

import Data.Text (unpack)

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    userMb <- userNameMb
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "MyCite"
        $(widgetFile "homepage")
        loginW userMb

userNameMb :: Handler (Maybe User)
userNameMb = do
  authIdMb <- maybeAuthId
  runDB $ do
    case authIdMb of
      Just key -> get key
      Nothing -> return Nothing

updateUser :: User -> YesodDB App App ()
updateUser user = do
  entityMb <- getByValue user
  case entityMb of
    Just entity -> replace (entityKey entity) user
    Nothing -> return ()

loginW :: Maybe User -> Widget
loginW userMb = [whamlet|
<div>
  $maybe user <- userMb
    <p>#{userIdent user}
      <ul>
        <li><a href=@{AuthR LogoutR}>Logout
        <li><a href=@{EditDetailsR}>Edit details
  $nothing
    <p><a href=@{AuthR LoginR}>Login</a>
|]


getEditDetailsR :: Handler RepHtml
getEditDetailsR = do
  userMb <- userNameMb
  case userMb of
    Just user -> do 
      (widget, enctype) <- generateFormPost (detailsForm user)
      defaultLayout
          [whamlet|
            <h1>Edit details
            <form method=post action=@{EditDetailsR} enctype=#{enctype}>
               ^{widget}
               <input type=submit>
          |]
    Nothing -> notLoggedIn
 
detailsForm :: User -> Form User
detailsForm (User email nameMb affilMb discpMb) = renderDivs $
  User email <$> aopt textField "Name" (Just nameMb)
             <*> aopt textField "Affiliation" (Just affilMb)
             <*> aopt textField "Discipline" (Just discpMb)

notLoggedIn :: Handler RepHtml
notLoggedIn = defaultLayout [whamlet|<p>You're not logged in.|]

postEditDetailsR :: Handler RepHtml
postEditDetailsR = do
  userMb <- userNameMb
  case userMb of 
    Just user -> do
      ((result, _widget), _enctype) <- runFormPost (detailsForm user)
      case result of
        FormSuccess user' -> do
          runDB (updateUser user')
          defaultLayout [whamlet|
<p>Saved information
|]
        _ -> defaultLayout [whamlet|
<p>Invalid input!
|]
    Nothing -> notLoggedIn

getPubsR :: Handler RepHtml
getPubsR = do
  allPubs <- runDB (selectList [] [])
  defaultLayout [whamlet|
<h1>Publications</h1>
  <ul>
    $forall entity <- allPubs
      <li>^{pubEntryLine entity}
|]

pubEntryLine :: Entity Pub -> Widget
pubEntryLine (Entity key (Pub title _abstract year _auths _pubtype)) = [whamlet|
   #{unpack title}, 
   published #{show year}, 
   <a href=@{EditRefR key}>(edit)</a>
|]
