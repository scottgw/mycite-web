module Handler.User where

import Import

getUserR userId = do
  defaultLayout [whamlet|User info|]