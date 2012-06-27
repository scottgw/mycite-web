module Handler.Util where

import Yesod.Auth

import Import

getUser :: Handler (Maybe User)
getUser = do
  authIdMb <- maybeAuthId
  runDB $ do
    case authIdMb of
      Just key -> get key
      Nothing -> return Nothing
