module Handler.AddEditReview where

import Import

getAddReviewR :: PubId -> Handler RepHtml
getAddReviewR _pubId = do
  defaultLayout [whamlet|Add review GET|]

postAddReviewR :: PubId -> Handler RepHtml
postAddReviewR _pubId = do
  defaultLayout [whamlet|Add review POST|]

getEditReviewR :: ReviewId -> Handler RepHtml
getEditReviewR _reviewId = do
  defaultLayout [whamlet|Edit review GET|]

postEditReviewR :: ReviewId -> Handler RepHtml
postEditReviewR _reviewId = do
  defaultLayout [whamlet|Edit review POST|]