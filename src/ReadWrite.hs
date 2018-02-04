{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module ReadWrite where

import           Model

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Data.Maybe
import           Data.Text                   hiding (head, map)
import           Data.Time
import           Database.Esqueleto
import           Database.Persist.Postgresql (withPostgresqlConn)

runDb ::
     (MonadIO m, MonadBaseControl IO m)
  => ReaderT SqlBackend (LoggingT m) a
  -> m a
runDb q = do
  let con = "host=localhost port=5432 user=ibnu dbname=bot password=jaran"
  runStderrLoggingT $ withPostgresqlConn con $ \b -> runReaderT q b

insertIncome :: Text -> Double -> IO (Key Income)
insertIncome source amount = do
  now <- getCurrentTime
  runDb $ insert $ Income source amount now

insertExpense :: Text -> Double -> IO (Key Expense)
insertExpense towhom amount = do
  now <- getCurrentTime
  runDb $ insert $ Expense towhom amount now

searchIncomeBySource ::
  (MonadBaseControl IO m, MonadIO m) => Text -> m [Income]
searchIncomeBySource source = do
  incomes <-
    runDb $
    select $
    from $ \inc -> do
      where_ (inc ^. IncomeSource ==. val source)
      limit 10
      orderBy [desc (inc ^. IncomeWhen)]
      return inc
  return $ map entityVal incomes

searchIncomeBySourceLimit ::
  (MonadBaseControl IO m, MonadIO m) => Text -> Double -> m [Income]
searchIncomeBySourceLimit source lim = do
  incomes <-
    runDb $
    select $
    from $ \inc -> do
      where_ (inc ^. IncomeSource ==. val source &&. inc ^. IncomeAmount <=. val lim)
      limit 10
      orderBy [desc (inc ^. IncomeWhen)]
      return inc
  return $ map entityVal incomes

searchExpenseBySource ::
  (MonadBaseControl IO m, MonadIO m) => Text -> m [Expense]
searchExpenseBySource source = do
  incomes <-
    runDb $
    select $
    from $ \inc -> do
      where_ (inc ^. ExpenseTowhom ==. val source)
      limit 10
      orderBy [desc (inc ^. ExpenseWhen)]
      return inc
  return $ map entityVal incomes

searchExpenseBySourceLimit ::
  (MonadBaseControl IO m, MonadIO m) => Text -> Double -> m [Expense]
searchExpenseBySourceLimit source lim = do
  expenses <-
    runDb $
    select $
    from $ \inc -> do
      where_ (inc ^. ExpenseTowhom ==. val source &&. inc ^. ExpenseAmount <=. val lim)
      limit 10
      orderBy [desc (inc ^. ExpenseWhen)]
      return inc
  return $ map entityVal expenses

anu ::
  (BaseBackend backend ~ SqlBackend, PersistUniqueRead backend,
   PersistQueryRead backend, IsPersistBackend backend, MonadIO m) =>
  Text -> Double -> ReaderT backend m [Value UTCTime]
anu source lim = do
  xx <-
    select $
    from $ \inc -> do
      where_ (inc ^. ExpenseTowhom ==. val source &&. inc ^. ExpenseAmount <=. val lim)
      limit 10
      orderBy [desc (inc ^. ExpenseWhen)]
      return $ inc ^. ExpenseWhen
  return xx

totalIncome ::
     ( BaseBackend backend ~ SqlBackend
     , PersistUniqueRead backend
     , PersistQueryRead backend
     , IsPersistBackend backend
     , MonadIO m
     )
  => ReaderT backend m Double
totalIncome = do
  anu <- select $ from $ \inc -> return $ joinV $ sum_ (inc ^. IncomeAmount)
  return $ head $ map (fromJust . unValue) anu

totalExpense ::
     ( BaseBackend backend ~ SqlBackend
     , PersistUniqueRead backend
     , PersistQueryRead backend
     , IsPersistBackend backend
     , MonadIO m
     )
  => ReaderT backend m Double
totalExpense = do
  anu <- select $ from $ \inc -> return $ joinV $ sum_ (inc ^. ExpenseAmount)
  return $ head $ map (fromJust . unValue) anu

balance :: IO Double
balance = do
  inc <- runDb totalIncome
  exp <- runDb totalExpense
  return $ inc - exp
