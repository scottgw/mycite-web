{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module MyCite.Model.Util where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time

import Database.Persist
import Database.Persist.GenericSql

import Prelude

import MyCite.Model.Definition

-- run :: ConnectionPool -> SqlPersist m a -> IO a
-- run = flip runSqlPool

class ( MonadIO m
      , Functor m
      , MonadBaseControl IO m
      , MonadUnsafeIO m
      , MonadThrow m
      ) => MonadDB m where

authorUserName :: MonadDB m => Author -> SqlPersist m Text
authorUserName author =
  case authorUser author of
     Just userKey -> userIdent <$> getJust userKey
     Nothing -> return $ fromMaybe (error "authorName: should not be Nothing") 
                                   (authorName author)

authorsFor' :: MonadDB m =>  Pub -> SqlPersist m [Text]
authorsFor' pub = mapM (authorUserName <=< getJust) (pubAuthors pub)

authorsFor :: MonadDB m => Pub -> SqlPersist m Text
authorsFor pub = Text.intercalate "; " <$> authorsFor' pub

reviewsFor :: MonadDB m => PubKey -> SqlPersist m [Entity Review]
reviewsFor key = selectList [ReviewRefer ==. key] []

yearToDay :: Integral a => a -> Day
yearToDay year = fromGregorian (fromIntegral year) 0 0