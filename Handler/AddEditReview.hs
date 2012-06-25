module Handler.AddEditReview where

import qualified Data.Text as Text

import MyCite.Model.Util

import Import

getAddReviewR :: PubId -> Handler RepHtml
getAddReviewR pubId = do
  reviews <- runDB $ reviewsFor pubId
  defaultLayout [whamlet|A|]

renderReview :: Review -> Handler Widget
renderReview review = do
  Just user <- runDB $ get (reviewUser review)
  let name = maybe (Text.pack $ show $ reviewUser review) id (userName user)
  return $ [whamlet|
    Review by <a href=@{UserR (reviewUser review)}>#{name}</a> <br>
    #{reviewText review} <br>
  |]

postAddReviewR :: PubId -> Handler RepHtml
postAddReviewR _pubId = do
  defaultLayout [whamlet|Add review POST|]

getEditReviewR :: ReviewId -> Handler RepHtml
getEditReviewR _reviewId = do
  defaultLayout [whamlet|Edit review GET|]

postEditReviewR :: ReviewId -> Handler RepHtml
postEditReviewR _reviewId = do
  defaultLayout [whamlet|Edit review POST|]