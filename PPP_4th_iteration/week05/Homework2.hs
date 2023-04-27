{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Homework2 where

import Plutus.V1.Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    MintingPolicy,
    ScriptContext (scriptContextTxInfo),
    TokenName,
    TxId (TxId),
    TxInfo,
    TxOutRef (TxOutRef),
    mkMintingPolicyScript,
    scriptContextTxInfo,
    txInInfoOutRef,
    txInfoInputs,
    txInfoMint,
    unTokenName,
  )
import qualified PlutusTx
import PlutusTx.Prelude (Bool (False), any, emptyByteString, traceIfFalse, ($), (&&), (.), (==))
import Utilities (wrapPolicy, writeCodeToFile)
import Prelude (IO)

{-# INLINEABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy _oref () _ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "TokenName not empty" hasEmptyTokenName
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == _oref) $ txInfoInputs info

    hasEmptyTokenName :: Bool
    hasEmptyTokenName = case flattenValue (txInfoMint info) of
      [(_, tn, amt)] -> unTokenName tn == emptyByteString && amt == 1
      _ -> False

{-# INLINEABLE mkWrappedEmptyNFTPolicy #-}
-- mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
-- mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy
mkWrappedEmptyNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy tid ix = wrapPolicy $ mkEmptyNFTPolicy oref
  where
    oref :: TxOutRef
    oref = TxOutRef (TxId $ PlutusTx.unsafeFromBuiltinData tid) (PlutusTx.unsafeFromBuiltinData ix)

emptyNFTCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
emptyNFTCode = $$(PlutusTx.compile [||mkWrappedEmptyNFTPolicy||])

-- nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
-- nftPolicy oref tn = mkMintingPolicyScript $ $$(PlutusTx.compile [||mkWrappedEmptyNFTPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode oref

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveEmptyNFTCode :: IO ()
saveEmptyNFTCode = writeCodeToFile "assets/emptyNFT.plutus" emptyNFTCode