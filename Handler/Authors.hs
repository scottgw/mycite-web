module Handler.Authors where

import Data.Maybe

import Import

getAuthorsR = do
  authors <- runDB $ selectList [] []
  defaultLayout [whamlet|
<h2>Authors</h2>
  <ol>
    $forall author <- authors
      <li>#{fromJust $ authorName $ entityVal author}
|]
                 