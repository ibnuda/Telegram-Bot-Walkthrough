{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           Data.Text
import           Data.Time
import           Database.Persist.Sql
import           Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Income
    source Text
    amount Double
    when   UTCTime
    deriving Show Eq
  Expense
    towhom Text
    amount Double
    when   UTCTime
    deriving Show Eq
  |]

doMigration :: SqlPersistT IO ()
doMigration = runMigration migrateAll
