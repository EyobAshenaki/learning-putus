module Transactions where

import qualified Data.Map as M
-- import Tables
import Prelude hiding (lookup)

-- START HERE AFTER Tables.hs

-- This is the transactions datatype from the slides,
-- using record syntax.

data Transaction = Transaction
  { trAmount :: Amount,
    trFrom :: Account,
    trTo :: Account
  }
  deriving (Eq, Show)

type Amount = Int

type Account = String

-- Both declarations below define transactions. The
-- first uses the 'Transaction' constructor normally,
-- the second uses record syntax during construction.
--
-- In the record version, we can assign the fields
-- in a different oder; otherwise, both versions are
-- equivalent.

transaction1 :: Transaction
transaction1 = Transaction 10 "Andres" "Lars"

transaction2 :: Transaction
transaction2 =
  Transaction
    { trAmount = 7,
      trFrom = "Lars",
      trTo = "Alejandro"
    }

-- Task Transactions-1.
--
-- Flip a transaction, by producing a transaction of
-- the negative amount in the opposite direction.

-- |
-- >>> flipTransaction transaction1
-- Transaction {trAmount = -10, trFrom = "Lars", trTo = "Andres"}
flipTransaction :: Transaction -> Transaction
flipTransaction (Transaction amt f t) = Transaction (-10) t f

-- Task Transactions-2.
--
-- Normalize a transaction by flipping it if and only
-- if the transaction amount is negative.

normalizeTransaction :: Transaction -> Transaction
normalizeTransaction txs@(Transaction amt f t) = if amt < 0 then flipTransaction (Transaction amt f t) else (Transaction amt f t)

-- Task Transactions-3.
--
-- Re-implement 'processTransaction' from the slides,
-- but use the function 'alter' that you defined in
-- the Tables tasks.

-- Account -> String, Amount -> Int
-- type Accounts = Table Account Amount
type Accounts = M.Map String Int

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe x Nothing = x

-- |
-- >>> let a = processTransaction transaction1 $ insert "Andres" 30 empty
-- >>> lookup "Andres" a
-- Just 20
-- >>> lookup "Lars" a
-- Just 10
-- >>> lookup "Alejandro" a
-- Nothing
-- >>> let b = processTransaction transaction2 a
-- >>> lookup "Lars" b
-- Just 3
-- >>> lookup "Alejandro" b
-- Just 7
processTransaction :: Transaction -> Accounts -> Accounts
processTransaction (Transaction amt from to) accounts =
  let fromOld = fromMaybe 0 (M.lookup from accounts)
      toOld = fromMaybe 0 (M.lookup to accounts)
   in M.insert from (fromOld - amt) (M.insert to (toOld + amt) accounts)

-- Task Transactions-4.
--
-- Verify that you can no longer pattern match on the
-- 'Tables' constructor if you have hidden the 'Tables'
-- constructor from the export list as requested in the
-- Tables tasks.

-- Task Transactions-5.
--
-- Process a list of transactions one by one.

-- |
-- >>> let a = processTransactions [transaction1, transaction2] empty
-- >>> lookup "Andres" a
-- Just (-10)
-- >>> lookup "Lars" a
-- Just 3
-- >>> lookup "Alejandro" a
-- Just 7
processTransactions :: [Transaction] -> Accounts -> Accounts
processTransactions [] accounts = accounts
processTransactions (t : ts) accounts = processTransactions ts (processTransaction t accounts)

-- Task Transactions-6.
--
-- Write a version of 'processTransaction' that fails
-- if and only if the new balances would be negative.

-- |
-- >>> let Just a = processTransaction' transaction1 $ insert "Andres" 30 empty
-- >>> lookup "Andres" a
-- Just 20
-- >>> lookup "Lars" a
-- Just 10
--
-- >>> processTransaction' transaction1 empty
-- Nothing
processTransaction' :: Transaction -> Accounts -> Maybe Accounts
processTransaction' (Transaction amt from to) accounts =
  let fromOld = fromMaybe 0 (M.lookup from accounts)
      toOld = fromMaybe 0 (M.lookup to accounts)
   in if fromOld >= amt then Just (M.insert from (fromOld - amt) (M.insert to (toOld + amt) accounts)) else Nothing

-- Task Transactions-7.
--
-- Write a version of 'processTransactions' that fails
-- if any of the individual transactions fail.

-- |
-- >>> let Just a = processTransactions' [transaction1, transaction2] $ insert "Andres" 50 empty
-- >>> lookup "Andres" a
-- Just 40
-- >>> lookup "Lars" a
-- Just 3
-- >>> lookup "Alejandro" a
-- Just 7
--
-- >>> processTransactions' [transaction1, transaction2] empty
-- Nothing
processTransactions' :: [Transaction] -> Accounts -> Maybe Accounts
processTransactions' [] accounts = Just accounts
processTransactions' (t : ts) accounts = let txs = processTransaction' t accounts in if txs == Nothing then Nothing else Just (processTransactions ts (processTransaction t accounts))

-- Task Transactions-8.
--
-- Make your package depend on the @containers@ package.
-- For this module, switch from using the 'Tables' datatype
-- to the 'Map' datatype from the 'Data.Map' module, and
-- verify that everything still works.

-- GO BACK to Datatypes.hs