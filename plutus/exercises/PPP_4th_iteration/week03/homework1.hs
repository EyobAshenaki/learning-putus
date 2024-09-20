{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Homework1 where

import Plutus.V1.Ledger.Interval (after, before)
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    POSIXTime,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    Validator,
    mkValidatorScript,
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.Prelude (Bool (..), otherwise, ($), (&&))
import Utilities (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
  { beneficiary1 :: PubKeyHash,
    beneficiary2 :: PubKeyHash,
    deadline :: POSIXTime
  }

unstableMakeIsData ''VestingDatum

{-# INLINEABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx
  | signedByBeneficiary1 && beforeDeadline = True
  | signedByBeneficiary2 && afterDeadline = True
  | otherwise = False
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    signedByBeneficiary1 :: Bool
    signedByBeneficiary1 = txSignedBy info $ beneficiary1 _dat

    signedByBeneficiary2 :: Bool
    signedByBeneficiary2 = txSignedBy info $ beneficiary2 _dat

    beforeDeadline :: Bool
    beforeDeadline = after (deadline _dat) $ txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = before (deadline _dat) $ txInfoValidRange info

{-# INLINEABLE mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [||mkWrappedVestingValidator||])
