{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Homework1 where

import Plutus.V1.Ledger.Interval (after)
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    MintingPolicy,
    POSIXTime,
    PubKeyHash,
    ScriptContext,
    TxInfo,
    mkMintingPolicyScript,
    scriptContextTxInfo,
    txInfoValidRange,
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import PlutusTx.Prelude (Bool, traceIfFalse, ($), (&&))
import Utilities (wrapPolicy, writeCodeToFile)
import Prelude (IO)

{-# INLINEABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy _pkh _deadline () _ctx =
  traceIfFalse "*** Invalid signature ***" signedByValidSignature
    && traceIfFalse "*** Deadline passed ***" deadlineNotReached
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    signedByValidSignature :: Bool
    signedByValidSignature = txSignedBy info _pkh

    deadlineNotReached :: Bool
    deadlineNotReached = after _deadline $ txInfoValidRange info

{-# INLINEABLE mkWrappedDeadlinePolicy #-}
-- mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
-- mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline
mkWrappedDeadlinePolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy (PlutusTx.unsafeFromBuiltinData pkh) (PlutusTx.unsafeFromBuiltinData deadline)

deadlineCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
deadlineCode = $$(PlutusTx.compile [||mkWrappedDeadlinePolicy||])

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline =
  mkMintingPolicyScript $
    deadlineCode
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ pkh)
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ deadline)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveDeadlineCode :: IO ()
saveDeadlineCode = writeCodeToFile "assets/deadline.plutus" deadlineCode