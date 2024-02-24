{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE OverloadedStrings   #-}  --Enable passing strings as other character formats, like bytestring.
{-# LANGUAGE RecordWildCards     #-}

module Lending where

--PlutusTx 
import                  PlutusTx                       (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed, unsafeFromBuiltinData)
import                  PlutusTx.Prelude               (traceIfFalse, otherwise, (==), Bool (..), Integer, ($), (>), (+), (&&))
import                  Plutus.V1.Ledger.Value      as PlutusV1
import                  Plutus.V1.Ledger.Interval      (contains, to, after) 
import                  Plutus.V2.Ledger.Api        as PlutusV2
import                  Plutus.V2.Ledger.Contexts      (txSignedBy, valueSpent)
--Serialization
import                  Wrappers                       (wrapValidator)
import                  Serialization                  (writeValidatorToFile, writeDataToFile)
import                  Prelude                        (IO, filter, fmap, mconcat, any, (<=), (||), (.), length)

--THE ON-CHAIN CODE
data LendingDatum = LendingDatum
    { borrowersNftTn        :: !TokenName
    , borrowersAddress      :: !Address
    , loan                  :: !AssetClass          --One of 3 test native tokens.
    , loanAmnt              :: !Integer             --Ammount of the Loan
    , interestAmnt          :: !Integer             --Will only be ADA
    , collateralAmnt        :: !Integer             --Will only be ADA
    , loanDuration          :: !POSIXTime           --Time constraint
    , lenderCommission      :: !Integer             --Lender Benefits
    , requestExpiration     :: !POSIXTime           --Time constraint
    , lenderNftTn           :: !TokenName           --LenderNFT
    , lendDate              :: !POSIXTime           --Timestamp
    }

PlutusTx.makeIsDataIndexed ''LendingDatum [('LendingDatum, 0)]

data ContractInfo = ContractInfo
    { lenderNftCs    :: !CurrencySymbol
    , borrowersNftCs :: !CurrencySymbol
    , collateralSc   :: !Address
    }

{-# INLINABLE lendingValidator #-}
lendingValidator :: ContractInfo -> LendingDatum -> TokenName -> ScriptContext -> Bool
lendingValidator contractInfo@ContractInfo{..} dat lenderTn ctx = validate
  where
    
    checkForTokensDos :: TxOut -> Bool
    checkForTokensDos txo = length ((flattenValue . txOutValue) txo) <= 3

    containsRequiredCollateralAmount :: TxOut -> Bool
    containsRequiredCollateralAmount txo =
      collateralAmnt dat <= assetClassValueOf (txOutValue txo) (AssetClass (adaSymbol, adaToken))


    isItToCollateral :: TxOut -> Bool
    isItToCollateral txo = txOutAddress txo == collateralSc    

    txOutValidate :: TxOut -> Bool
    txOutValidate txo =
      isItToCollateral txo &&
      containsRequiredCollateralAmount txo &&
      checkForTokensDos txo

    validateBorrowerMint :: Bool
    validateBorrowerMint = case mintFlattened ctx of
      [(cs, tn, amt)] -> (cs == borrowersNftCs) &&
                         (tn == borrowersNftTn dat) &&
                         (amt == (-1))
      _               -> False

    validateExpiration :: Bool
    validateExpiration = after (requestExpiration dat) (range ctx)

    validateMint :: Bool
    validateMint = case mintFlattened ctx of
      [(cs, tn, amt)] -> (cs == lenderNftCs) &&
                         (tn == lenderTn) &&
                         (amt == 1)
      _               -> False

    borrowerGetsWhatHeWants :: Bool
    borrowerGetsWhatHeWants =
      assetClassValueOf (valuePaidToAddress ctx (borrowersAddress dat)) (loan dat)
      == loanAmnt dat

    validateTxOuts :: Bool
    validateTxOuts = any txOutValidate (txInfoOutputs $ info ctx)

    validate :: Bool
    validate =
      validateTxOuts &&
      borrowerGetsWhatHeWants &&
      validateMint &&
      validateExpiration ||
      validateBorrowerMint

{-# INLINEABLE valuePaidToAddress #-}
valuePaidToAddress :: ScriptContext -> Address -> Value
valuePaidToAddress ctx addr = mconcat
  (fmap txOutValue (filter (\x -> txOutAddress x == addr)
  (txInfoOutputs (info ctx))))

{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo

{-# INLINEABLE mintFlattened #-}
mintFlattened :: ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintFlattened ctx = flattenValue $ txInfoMint (info ctx)

{-# INLINEABLE range #-}
range :: ScriptContext -> POSIXTimeRange
range ctx = txInfoValidRange (info ctx)

{-# INLINABLE wrappedLendingValidator #-}
wrappedLendingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData  -> BuiltinData -> ()
wrappedLendingValidator lendersNFT borrowersNFT collateralSA = wrapValidator $ lendingValidator contract
     where
         contract :: ContractInfo
         contract = ContractInfo (lenderNftCs   $ PlutusTx.unsafeFromBuiltinData lendersNFT)
                                 (borrowerNftCs $ PlutusTx.unsafeFromBuiltinData borrowersNFT)
                                 (collateralSc  $ PlutusTx.unsafeFromBuiltinData collateralSA)

lendingValidatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData  -> BuiltinData -> ())
lendingValidatorCode = $$(PlutusTx.compile [|| wrappedLendingValidator ||])


-- -- lendingVal :: ContractInfo -> Validator
-- lendingVal contract = mk
-- bountyNFTPolicy :: TxOutRef -> MintingPolicy
-- bountyNFTPolicy utxoRef = mkMintingPolicyScript $
--                           bountyNFTcode
--                            `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId utxoRef)
--                            `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx utxoRef)


--  mappedMathBounty :: BuiltinData -> BuiltinData -> BuiltinData -> ()
--  mappedMathBounty = wrapValidator mathBounty

-- mathBountyValidator :: Validator
-- mathBountyValidator =  PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedMathBounty ||])

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

{- Serialised Scripts and Values -}

-- saveMathBountyValidator :: IO ()
-- saveMathBountyValidator =  writeValidatorToFile "./testnet/mathBounty.plutus" mathBountyValidator

-- saveDatum :: IO ()
-- saveDatum = writeDataToFile "./testnet/bountyConditions.json" BC { theX = 5
--                                                                  , deadline = 1688738400000
--                                                                  }

saveTheY :: IO ()
saveTheY = writeDataToFile "./testnet/value-5.json" (-5::Integer) 

saveAll :: IO ()
saveAll = do
            saveTheY
