{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleNFT where

import qualified PlutusTx
import           PlutusTx.Prelude           (Bool (..) , Eq ((==)), any, traceIfFalse, ($), (&&))
import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData, CurrencySymbol,
                                             MintingPolicy,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxId (TxId, getTxId),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript)
import           Plutus.V2.Ledger.Api     as PlutusV2
import           Prelude              (IO)
import           Wrappers          (wrapPolicy)
import           Serialization    (currencySymbol, writePolicyToFile, writeDataToFile) 
import GHC.Base (IO)

--THE ON-CHAIN CODE

{-# INLINABLE eaNFT #-}
simpleNFT :: Bool -> ScriptContext -> Bool
simpleNFT category sContext = case category of
        True  -> forging
        False -> burning        
    where
        forging = traceIfFalse "UTxO not available!" hasUTxO &&
                  traceIfFalse "There can be only ONE!" checkMintedAmount

        burning = traceIfFalse "Only burning one, nothing more, nothing less!" checkBurnedAmount 

        hasUTxO :: Bool
        hasUTxO = any (\x -> txInInfoOutRef x == utxo) $ txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
            [(_, _, amt)] -> amt == 1
            _             -> False

        checkBurnedAmount :: Bool
        checkBurnedAmount = case flattenValue (txInfoMint info) of
            [(_, _, amt)] -> amt == -1
            _             -> False

        info :: TxInfo
        info = scriptContextTxInfo sContext

{-# INLINABLE wrappediEAnftPolicy #-}
wrappediEAnftPolicy :: BuiltinData -> BuiltinData -> BuiltinData  -> BuiltinData -> ()
wrappediEAnftPolicy utxoId utxoIx = wrapPolicy $ ieaNFT utxo
    where
        utxo :: TxOutRef
        utxo = TxOutRef (TxId $ PlutusTx.unsafeFromBuiltinData utxoId) (PlutusTx.unsafeFromBuiltinData utxoIx)

ieaNFTcode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
ieaNFTcode = $$(PlutusTx.compile [|| wrappediEAnftPolicy ||])

ieaNFTPolicy :: TxOutRef -> MintingPolicy
ieaNFTPolicy utxoRef = mkMintingPolicyScript $
                          ieaNFTcode
                           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId utxoRef)
                           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx utxoRef)

-- The complete minting policy validator version

wrappedEAnftPolicy :: BuiltinData -> BuiltinData -> BuiltinData  -> BuiltinData -> ()
wrappedEAnftPolicy utxoId utxoIx = wrapPolicy $ eaNFT utxo
    where
        utxo :: TxOutRef
        utxo = TxOutRef (TxId $ PlutusTx.unsafeFromBuiltinData utxoId) (PlutusTx.unsafeFromBuiltinData utxoIx)

eaNFTcode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
eaNFTcode = $$(PlutusTx.compile [|| wrappedEAnftPolicy ||])

eaNFTPolicy :: TxOutRef -> MintingPolicy
eaNFTPolicy utxoRef = mkMintingPolicyScript $
                          eaNFTcode
                           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId utxoRef)
                           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx utxoRef)

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

{- Serialised Scripts and Values -}

param :: TxOutRef
param  = PlutusV2.TxOutRef { txOutRefId = "3b3c87bc71d72d169af15bf7dd5f1793bc9a40ab6eadcf2d3b3cc66e8ae4de6a"
                           , txOutRefIdx = 0}
param2 :: TxOutRef
param2  = PlutusV2.TxOutRef { txOutRefId = "cbe4eb5f4b9ac4be54bb75b2415fd1b0d29f4757e8dd0d86d5260d92c0264118"
                           , txOutRefIdx = 0}
-- You have to provide your own UTxO TxID and Index on serialization of the minting policy validator.                       

saveieaNFTPolicy :: IO ()
saveieaNFTPolicy =  writePolicyToFile "./testnet/ieaNFT.plutus" $ ieaNFTPolicy param   

saveeaNFTPolicy :: IO ()
saveeaNFTPolicy =  writePolicyToFile "./testnet/eaNFT.plutus" $ eaNFTPolicy param2   

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveRedeemerForging :: IO ()
saveRedeemerForging = writeDataToFile "./testnet/Forge.json" True

saveRedeemerBurning :: IO ()
saveRedeemerBurning = writeDataToFile "./testnet/Burn.json" False

saveAll :: IO ()
saveAll = do
            saveieaNFTPolicy
            saveeaNFTPolicy
            saveUnit
            saveRedeemerForging
            saveRedeemerBurning