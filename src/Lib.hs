module Lib
  ( someFunc
  ) where

import           Model
import           ReadWrite

import           Control.Monad.Logger
import           Data.Maybe
import           Data.Text
import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser

data ChatContent
  = IncomeOrExpense Text Double
  | Other Text
  | EmptyContent
  deriving (Show, Eq)

data ChatModel =
  ChatModel ChatContent
  deriving (Show, Eq)

emptyChatModel = ChatModel EmptyContent

data Action
  = Empty
  | ActHelp
  | ActAddInc
  | ActAddExp
  | ActMessageText Text
  | ActMessageDouble Double
  deriving (Show, Eq)

incexpBotApp :: BotApp ChatModel Action
incexpBotApp = BotApp
  { botInitialModel = emptyChatModel
  , botAction = flip updateToAction
  , botHandler = updateHandler
  , botJobs = []
  }

updateToAction = undefined
updateHandler = undefined

someFunc :: IO ()
someFunc = putStrLn "text"
