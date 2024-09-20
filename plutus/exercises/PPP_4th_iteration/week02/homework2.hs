{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Homework2 where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.Prelude (Bool, BuiltinData, Eq ((==)), not, traceIfFalse, ($))
import Utilities (wrapValidator)

-- import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data MyRedeemer = MyRedeemer
  { flag1 :: Bool,
    flag2 :: Bool
  }

PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINEABLE mkValidator #-}
-- Create a validator that unlocks the funds if MyRedemeer's flags are different
mkValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
mkValidator _ (MyRedeemer {flag1 = f1, flag2 = f2}) _ = traceIfFalse "Wrong Redeemer" $ not $ f1 == f2

{-# INLINEABLE wrappedVal #-}
wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [||wrappedVal||])
