{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Home where

import Yesod.Auth

import Data.Text (unpack)

import Handler.Util
import Import

-- | Html for the homepage.
getHomeR :: Handler RepHtml
getHomeR = do
    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    userMb <- getUser
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "MyCite"
        $(widgetFile "homepage")
        loginW userMb

-- | Login widget, takes a possible user as an argument.
-- If the user exists, then links to logout and edit the user details.
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

-- | Display the publications as a list.
getPubsR :: Handler RepHtml
getPubsR = do
  allPubs <- runDB (selectList [] [])
  defaultLayout [whamlet|
<h1>Publications</h1>
  <ul>
    $forall entity <- allPubs
      <li>^{pubEntryLine entity}
|]

-- | A widget for a single publication entry.
pubEntryLine :: Entity Pub -> Widget
pubEntryLine (Entity key (Pub title _abstract year _auths _pubtype)) = [whamlet|
   #{unpack title}, 
   published #{show year}, 
   <a href=@{EditRefR key}>(edit)</a>
|]
