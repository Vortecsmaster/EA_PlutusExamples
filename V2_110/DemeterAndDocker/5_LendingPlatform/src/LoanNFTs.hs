{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module LoanNFTs where

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

{-# INLINABLE bountyNFT #-}
bountyNFT :: TxOutRef -> () -> ScriptContext -> Bool
bountyNFT utxo _ sContext = traceIfFalse "UTxO not available!" hasUTxO &&
                            traceIfFalse "There can be only ONE!" checkMintedAmount
        
    where
        hasUTxO :: Bool
        hasUTxO = any (\x -> txInInfoOutRef x == utxo) $ txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
            [(_, _, amt)] -> amt == 1
            _             -> False

        info :: TxInfo
        info = scriptContextTxInfo sContext

{-# INLINABLE wrappedBountyNFTPolicy #-}
wrappedBountyNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData  -> BuiltinData -> ()
wrappedBountyNFTPolicy utxoId utxoIx = wrapPolicy $ bountyNFT utxo
    where
        utxo :: TxOutRef
        utxo = TxOutRef (TxId $ PlutusTx.unsafeFromBuiltinData utxoId) (PlutusTx.unsafeFromBuiltinData utxoIx)

bountyNFTcode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
bountyNFTcode = $$(PlutusTx.compile [|| wrappedBountyNFTPolicy ||])

bountyNFTPolicy :: TxOutRef -> MintingPolicy
bountyNFTPolicy utxoRef = mkMintingPolicyScript $
                          bountyNFTcode
                           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId utxoRef)
                           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx utxoRef)



------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

{- Serialised Scripts and Values -}

param :: TxOutRef
param  = PlutusV2.TxOutRef { txOutRefId = "3b3c87bc71d72d169af15bf7dd5f1793bc9a40ab6eadcf2d3b3cc66e8ae4de6a" --Must be one of the mathBounty UTxOs to be consumed
                           , txOutRefIdx = 0}

-- You have to provide your own UTxO TxID and Index on serialization of the minting policy validator.                       

saveBountyNFTPolicy :: IO ()
saveBountyNFTPolicy =  writePolicyToFile "./testnet/bountyNFT.plutus" $ bountyNFTPolicy param   

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveAll :: IO ()
saveAll = do
            saveBountyNFTPolicy
            saveUnit
            