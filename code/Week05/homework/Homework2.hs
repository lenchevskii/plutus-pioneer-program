{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V1.Ledger.Value (flattenValue, TokenName (TokenName))
import           Plutus.V2.Ledger.Api   (BuiltinData, MintingPolicy,
                                         ScriptContext (scriptContextTxInfo), TxInInfo (txInInfoOutRef),
                                         TxInfo (txInfoInputs, txInfoMint),
                                         TxOutRef, mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude       (Bool (False), Eq ((==)), any,
                                         ($), (&&), (.), emptyByteString)
import           Utilities              (wrapPolicy)

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy _oref () _ctx = hasUTxO && checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == _oref) $ txInfoInputs info
    
    tn :: TokenName
    tn = TokenName emptyByteString


    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == tn && amt == 1
        _                -> False

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $ $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref
