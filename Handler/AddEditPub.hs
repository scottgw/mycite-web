{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.AddEditPub where

import Control.Monad

import Data.Maybe
import Data.Text (pack)

import MyCite.Model.Util

import Yesod.Form.Jquery

import Import

instance YesodJquery App

instance MonadDB (GHandler App App)

editRefForm :: Maybe Pub -> [Text] -> Form Pub
editRefForm pubMb authorNames = renderDivs $
    Pub <$> areq textField "Title" (pull pubTitle)
        <*> (fmap unTextarea <$> aopt textareaField "Abstract" (pull (fmap Textarea . pubAbstract)))
        <*> areq (jqueryDayField def) "Date" (pull pubDate)
--        <*> pure [] -- ((:) <$> areq textField "Author" (listToMaybe authorNames))
        <*> formToAForm authorField
        <*> areq pubTypeField "Type of publication" (pull pubPubType)
  where
    authorField :: MForm App App (FormResult [AuthorKey], [FieldView App App])
    authorField = do
      (res, view) <- mreq textField "Author" (listToMaybe authorNames)
      res' <- case res of
        FormSuccess name -> lift (authorNameDB name)
        FormFailure reasons -> return (FormFailure reasons)
        FormMissing -> return FormMissing
      return (res', [view])

    authorNameDB :: Text -> GHandler App App (FormResult [AuthorKey])
    authorNameDB name = runDB $ do
      es <- selectList [AuthorName ==. Just name] []
      case es of
        [] -> -- return (FormFailure ["Author name not found"])
          insert (Author (Just name) Nothing) >>= 
          return . FormSuccess . (:[])
        (e:_es) ->  return (FormSuccess [entityKey e])
    pull :: (Pub -> a) -> Maybe a
    pull = (<$> pubMb)

getAddRefR :: Handler RepHtml
getAddRefR = getRef Nothing

postAddRefR :: Handler RepHtml
postAddRefR = 
  postRef Nothing 
    (\pub -> do
       runDB (void $ insert pub)
       defaultLayout [whamlet|<h1>Input valid!|])

getEditRefR :: PubId -> Handler RepHtml
getEditRefR pubId = getRef (Just pubId)

postEditRefR :: PubId -> Handler RepHtml
postEditRefR pubId =
  postRef (Just pubId)
    (\pub -> do
       runDB (replace pubId pub)
       defaultLayout [whamlet|<h1>Input valid, updated entry|])

pubAndAuthors :: Maybe PubId -> YesodDB App App (Maybe Pub, [Text])
pubAndAuthors Nothing = return (Nothing, [])
pubAndAuthors (Just pubId) = do
  pubMb <- get pubId
  case pubMb of
    Nothing -> return (Nothing, [])
    Just pub -> (Just pub,) <$> authorsFor' pub

getRef :: Maybe PubId -> Handler RepHtml
getRef pubIdMb = do
  (pubMb, authors) <- runDB (pubAndAuthors pubIdMb)
  (widget, enctype) <- generateFormPost (editRefForm pubMb authors)
  defaultLayout [whamlet|
<h1>Edit publication</h1>
^{editForm pubIdMb enctype widget}
|]

postRef :: Maybe PubId -> (Pub -> Handler RepHtml) -> Handler RepHtml
postRef pubIdMb f = do
  (pubMb, authors) <- runDB (pubAndAuthors pubIdMb)
  ((result, _widget), _enctype) <- runFormPost (editRefForm pubMb authors)
  case result of 
    FormSuccess pub -> f pub
    FormFailure reasons -> defaultLayout (pubSubmitFailure reasons)
    _ -> defaultLayout pubSubmitIncom

pubSubmitFailure :: [Text] -> Widget
pubSubmitFailure reasons = [whamlet|<h1>Input invalid! #{show reasons}|]

pubSubmitIncom :: Widget
pubSubmitIncom = [whamlet|<h1>Other error reason|]

editForm :: Maybe PubId -> Enctype -> Widget -> Widget
editForm Nothing      enctype widget = [whamlet|
<form method=post action=@{AddRefR} enctype=#{enctype}>
   ^{widget}
   <input type=submit>
|]
editForm (Just pubId) enctype widget = [whamlet|
<form method=post action=@{EditRefR pubId} enctype=#{enctype}>
   ^{widget}
   <input type=submit>
|]

pubTypeField :: Field App App PubType
pubTypeField = 
  selectFieldList 
    [(pack (show pubType), pubType)| pubType <- [Journal .. PhdThesis]]
