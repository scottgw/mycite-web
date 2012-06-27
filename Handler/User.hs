module Handler.User where

import Data.Maybe

import Handler.Util

import Import

getUserR :: UserId -> Handler RepHtml
getUserR userId = do
  userMb <- runDB $ get userId
  defaultLayout (maybe noUser userView userMb)
  
noUser :: Widget
noUser = [whamlet|Sorry, no user with that ID here!|]

userView :: User -> Widget
userView user = 
  [whamlet|
   <table>
     <tr>
       <td><strong>Name:
       <td>#{fromMaybe "No username given" (userName user)}
     <tr>   
       <td><strong>Affiliation:
       <td>#{fromMaybe "No affiliation given" (userAffiliation user)}
     <tr>
       <td><strong>Discipline:
       <td>#{fromMaybe "No discipline given" (userDiscipline user)}
          |]

updateUser :: User -> YesodDB App App ()
updateUser user = do
  entityMb <- getByValue user
  case entityMb of
    Just entity -> replace (entityKey entity) user
    Nothing -> return ()

getEditDetailsR :: Handler RepHtml
getEditDetailsR = do
  userMb <- getUser
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
  userMb <- getUser
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
