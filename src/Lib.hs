module Lib
  ( someFunc
  ) where

import           Model
import           ReadWrite

import           Data.Text

data ChatContent
  = IncomeOrExpense Text Double
  | JustOtherParty Text
  deriving (Show, Eq)

data ChatModel = ChatModel
  { content :: ChatContent
  } deriving (Show, Eq)

data Action
  = Empty
  | ActHelp
  | ActAddInc
  | ActAddExp
  | ActMessageText Text
  | ActMessageDouble Double
  deriving (Show, Eq)

someFunc :: IO ()
someFunc = putStrLn "text"
