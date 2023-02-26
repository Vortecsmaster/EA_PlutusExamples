{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module MathBountyV2o where

--PlutusTx 
import           PlutusTx                       (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins              as Builtins
import           PlutusTx.Prelude               hiding (Semigroup(..), unless, (.))
           
--Ledger 
import           Ledger                         hiding (singleton)
import qualified Ledger.Address                 as V1Address
import           Ledger.Constraints             as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Scripts           as UScripts
import           Ledger.Typed.Scripts.Validators
import           Plutus.V2.Ledger.Contexts      as LedgerV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts            -- New library name for Typed Validators and some new fuctions
import qualified Plutus.V2.Ledger.Api                 as PlutusV2            
import           Ledger.Ada                     as Ada
import           Ledger
--import for Serialization
import           Cardano.Api                          (PlutusScript, PlutusScriptV2, writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import          Codec.Serialise          
import           Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Prelude                              (FilePath, IO, Semigroup (..), Show (..), print, (.))
import qualified Plutus.V1.Ledger.Api                 as PlutusV1

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

data MathBountyDatum = MBD 
                     { mbBounty    :: Integer
                     , mbDeadline :: POSIXTime }  

PlutusTx.makeIsDataIndexed ''MathBountyDatum [('MBD,0)] 

{-# INLINABLE mathBountyValidator  #-}
mathBountyValidator :: MathBountyDatum -> Integer -> PlutusV2.ScriptContext -> Bool
mathBountyValidator datum x sContext = traceIfFalse "Wrong guess!" ((mbBounty datum) == x*x) &&
                                       traceIfFalse "Deadline passed!" deadlineReached
       where
            info :: LedgerV2.TxInfo
            info = LedgerV2.scriptContextTxInfo sContext

            deadlineReached :: Bool
            deadlineReached = contains (to $ mbDeadline datum) $ PlutusV2.txInfoValidRange info
                           -- (to $ mbDeadline datum) `contains` (txInfoValidRange info)

data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = MathBountyDatum

bountyValidator :: Scripts.TypedValidator MathBounty
bountyValidator = Scripts.mkTypedValidator @MathBounty
    $$(PlutusTx.compile [|| mathBountyValidator ||])
    $$(PlutusTx.compile [|| wrapping ||])
     where
       wrapping  = UScripts.mkUntypedValidator

validator :: Validator
validator = Scripts.validatorScript bountyValidator                   

{- SERIALIZATION -}

{- As a Short Byte String -}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise validator

{- As a Serialised Script -}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeMB :: IO ()
writeMB = void $ writeFileTextEnvelope "./testnet/MathBounty.plutus" Nothing serialisedScript

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . A.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusV1.toData

writeBadRedeemer :: IO ()
writeBadRedeemer = writeJSON "./testnet/value20.json" (20 :: Integer)

writeRedeemer1 :: IO ()
writeRedeemer1 = writeJSON "./testnet/value10.json" (10 :: Integer)

writeRedeemer2 :: IO ()
writeRedeemer2 = writeJSON "./testnet/value12.json" (12 :: Integer)

writeUnit :: IO ()
writeUnit = writeJSON "./testnet/unit.json" ()

writeD1 :: IO ()
writeD1 = writeJSON "./testnet/d1.json" (MBD 100 1677426600000)

writeD2 :: IO ()
writeD2 = writeJSON "./testnet/d2.json" (MBD 144 1677456600000)

