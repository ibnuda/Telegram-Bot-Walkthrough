module Lib
  ( someFunc
  ) where

import           Model
import           ReadWrite

import           Control.Applicative              ((<|>))

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Maybe
import           Data.Text
import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser

data ChatState
  = IncomeOrExpense Text Double
  | InsertingIncome Text
  | InsertingExpense Text
  | SearchingIncome
  | SearchingExpense
  | CheckingBalance
  | Other Text
  | EmptyContent
  deriving (Show, Eq)

data ChatModel =
  ChatModel ChatState
  deriving (Show, Eq)

emptyChatModel = ChatModel EmptyContent

data Action
  = Empty
  | ActHelp
  | ActBalance
  | ActAddInc
  | ActAddExp
  | ActSearchIncome
  | ActSearchExpense
  | ActMessage Text
  deriving (Show, Read)

incexpBotApp :: BotApp ChatModel Action
incexpBotApp = BotApp
  { botInitialModel = emptyChatModel
  , botAction = flip updateToAction
  , botHandler = updateHandler
  , botJobs = []
  }

updateToAction :: ChatModel -> Update -> Maybe Action
updateToAction _ =
  parseUpdate $
  ActHelp <$ command (pack "help") <|>
  ActBalance <$ command (pack "balance") <|>
  ActAddInc <$ command (pack "income") <|>
  ActAddExp <$ command (pack "expense") <|>
  ActSearchIncome <$ command (pack "incomes") <|>
  ActSearchExpense <$ command (pack "expenses") <|>
  ActMessage <$> plainText <|>
  callbackQueryDataRead

updateHandler :: Action -> ChatModel -> Eff Action ChatModel
updateHandler act model =
  case act of
    Empty -> pure model
    ActHelp ->
      emptyChatModel <# do
        reply . toReplyMessage . pack $ "/help"
        pure Empty
    ActBalance ->
      emptyChatModel <# do
        -- I feel smart!
        liftIO balance >>= reply . toReplyMessage . pack . show
        pure Empty

someFunc :: IO ()
someFunc = putStrLn "text"
