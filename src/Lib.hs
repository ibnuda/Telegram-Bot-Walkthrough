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
import           Text.Read                        (readMaybe)

data ChatState
  = IncomeOrExpense Text Double
  | InsertingIncome
  | InsertingIncomeSavedSource Text
  | InsertingExpense
  | InsertingExpenseSavedSource Text
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

replyString :: String -> BotM ()
replyString str = reply . toReplyMessage . pack $ str

updateHandler :: Action -> ChatModel -> Eff Action ChatModel
updateHandler act model =
  case act of
    Empty -> pure model
    ActHelp ->
      emptyChatModel <# do
        replyString "/help"
        pure Empty
    ActBalance ->
      emptyChatModel <# do
        liftIO balance >>= replyString . show -- I feel smart!
        pure Empty
    ActAddInc ->
      ChatModel InsertingIncome <# do
        replyString "Who gave you the money?"
        pure Empty
    ActAddExp ->
      ChatModel InsertingExpense <# do
        replyString "Who did you give it to?"
        pure Empty
    ActSearchIncome ->
      ChatModel SearchingIncome <# do
        replyString "Who are you looking for?"
        pure Empty
    ActSearchExpense ->
      ChatModel SearchingExpense <# do
        replyString "Who are you looking for?"
        pure Empty
    ActMessage msg -> textMessageHandler msg model

textToDoubleHandler msg chatmodel cont f =
  case (readMaybe . unpack $ msg :: Maybe Double) of
    Nothing ->
      chatmodel <# do
        replyString cont
        pure Empty
    Just amount -> f amount chatmodel

textMessageHandler :: Text -> ChatModel -> Eff Action ChatModel
textMessageHandler msg (ChatModel InsertingIncome) =
  textToDoubleHandler
    msg
    (ChatModel InsertingIncome)
    "You should input only numbers!"
    doubleMessageHandler
textMessageHandler msg (ChatModel InsertingExpense) =
  textToDoubleHandler
    msg
    (ChatModel InsertingExpense)
    "you should input only number"
    doubleMessageHandler

-- Should be extensible.
-- Perhaps passable function.
doubleMessageHandler amount (ChatModel (InsertingIncomeSavedSource source)) =
  ChatModel EmptyContent <# do
    _ <- liftIO $ insertIncome source amount
    replyString "Saved, mate!"
    pure Empty

someFunc :: IO ()
someFunc = putStrLn "text"
