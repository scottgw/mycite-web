{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
module MyCite.Model.Definition where

import Data.Text
import Data.Time

import Database.Persist
import Database.Persist.GenericSql
import Database.Persist.TH

import Prelude

data PubType = 
  Journal 
  | Conference 
  | Book 
  | Misc 
  | Article 
  | TechReport 
  | PhdThesis 
  deriving (Show, Read, Eq, Enum)

data Rating =
  Rating 
  { ratingWriting :: Int
  , ratingContent :: Int
  } 
  deriving (Show, Read, Eq)

derivePersistField "PubType"
derivePersistField "Rating"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Author
  name Text Maybe
  user UserKey Maybe

User
  ident Text
  name Text Maybe
  affiliation Text Maybe
  discipline Text Maybe
  UniqueUser ident

Pub
   title Text
   abstract Text Maybe
   date Day
   authors [AuthorKey]
   pubType PubType

Review
  refer PubKey
  user UserId
  rating Rating
  text Text
|]

type SqlKey a = Key SqlPersist a
type AuthorKey = SqlKey Author
type PubKey = SqlKey Pub
type UserKey = SqlKey User
