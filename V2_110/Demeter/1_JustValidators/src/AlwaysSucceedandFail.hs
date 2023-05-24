{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE FlexibleContexts    #-}  --Enable flexible contexts. Implied by ImplicitParams
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE ScopedTypeVariables #-}  --Enable lexical scoping of type variables explicit introduced with forall
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE TypeApplications    #-}  --Allow the use of type application syntax
{-# LANGUAGE TypeFamilies        #-}  --Allow use and definition of indexed type and data families
{-# LANGUAGE TypeOperators       #-}  --Allow the use and definition of types with operator names

module AlwaysSucceedandFail where

--PlutusTx 
import           PlutusTx                       (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins              as Builtins
import           PlutusTx.Prelude               hiding (Semigroup(..), unless, (.))
--Contract Monad
import           Plutus.Contract               
--Ledger 
import           Ledger                         hiding (singleton)
import           Plutus.V1.Ledger.Address       as V1Address
--import qualified Ledger.Address                 as V1Address
import           Ledger.Constraints             as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Scripts               as Scripts               
import qualified Plutus.Script.Utils.V2.Scripts as Scripts                  -- New library name for Typed Validators and some new fuctions
import           Ledger.Ada                     as Ada 
--import for Serialization
import           Cardano.Api                          (PlutusScript, PlutusScriptV2, writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise          
import           Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Prelude                              (FilePath, IO, Semigroup (..), Show (..), print, (.))
import qualified Plutus.V1.Ledger.Api                 as PlutusV1

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE


{-# INLINABLE alwaysSucceeds #-}                                    -- Everything that its supposed to run in on-chain code need this pragma
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()   -- the value of this function is on its sideeffects
alwaysSucceeds _ _ _ = () 

{-# INLINABLE alwaysFails #-}
alwaysFails :: BuiltinData -> BuiltinData -> BuiltinData -> ()   
alwaysFails _ _ _ = error () 


{-# INLINABLE aTest #-}
aTest :: BuiltinData -> BuiltinData -> BuiltinData -> ()
aTest datum  redeemer _ 
 | datum == redeemer   = ()
 | otherwise           = error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])  

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator  

scrAddress :: V1Address.Address
scrAddress = V1Address.scriptHashAddress valHash          -- Couldn't find a new version of scriptAddress for unTyped Scripts
--replaces:
--scrAddress = scriptAddress validator 

validator1 :: Validator
validator1 = mkValidatorScript $$(PlutusTx.compile [|| alwaysFails ||])  

valHash1 :: Ledger.ValidatorHash
valHash1= Scripts.validatorHash validator1 

scrAddress1 :: V1Address.Address
scrAddress1 = V1Address.scriptHashAddress valHash1  

validator2 :: Validator
validator2 = mkValidatorScript $$(PlutusTx.compile [|| aTest ||])  

valHash2 :: Ledger.ValidatorHash
valHash2 = Scripts.validatorHash validator2  

scrAddress2 :: V1Address.Address
scrAddress2 = V1Address.scriptHashAddress valHash2          -- Couldn't find a new version of scriptAddress for unTyped Scripts


{- As a Short Byte String -}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise validator

scriptSBS1 :: SBS.ShortByteString
scriptSBS1 = SBS.toShort . LBS.toStrict $ serialise validator1

scriptSBS2 :: SBS.ShortByteString
scriptSBS2 = SBS.toShort . LBS.toStrict $ serialise validator2

{- As a Serialised Script -}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeAS :: IO ()
writeAS = void $ writeFileTextEnvelope "./testnet/AS.plutus" Nothing serialisedScript

serialisedScript1 :: PlutusScript PlutusScriptV2
serialisedScript1 = PlutusScriptSerialised scriptSBS1

writeAF :: IO ()
writeAF = void $ writeFileTextEnvelope "./testnet/AF.plutus" Nothing serialisedScript1

serialisedScript2 :: PlutusScript PlutusScriptV2
serialisedScript2 = PlutusScriptSerialised scriptSBS2

writeDvR :: IO ()
writeDvR = void $ writeFileTextEnvelope "./testnet/DvR.plutus" Nothing serialisedScript2

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . A.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusV1.toData

writeUnit :: IO ()
writeUnit = writeJSON "./testnet/unit.json" ()

write19 :: IO ()
write19 = writeJSON "./testnet/value19.json" (19 :: Integer)