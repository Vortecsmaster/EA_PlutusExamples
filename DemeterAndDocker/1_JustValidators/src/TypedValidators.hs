{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE OverloadedStrings   #-}  --Enable passing strings as other character formats, like bytestring.

module TypedValidators where

--PlutusTx 
import                  PlutusTx                       (BuiltinData, compile,unstableMakeIsData, makeIsDataIndexed)
import                  PlutusTx.Prelude               (traceIfFalse, otherwise, (==), Bool (..), Integer, ($),(&&))
import                  Plutus.V2.Ledger.Api        as PlutusV2
--Serialization
import                  Wrappers                        (wrapValidator)
import                  Serialization                  (writeValidatorToFile, writeDataToFile)
import                  Prelude                     (IO)

------------------------------------------------------------------------------------------
-- Primites Types
------------------------------------------------------------------------------------------

{-# INLINABLE typedDatum22 #-}
typedDatum22 :: Integer -> () -> ScriptContext -> Bool
typedDatum22 datum _ _ = traceIfFalse "Not the right datum!" (datum ==  22)

{-# INLINABLE typedRedeemer11 #-}
typedRedeemer11 ::  () -> Integer -> ScriptContext -> Bool
typedRedeemer11 _ redeemer _ = traceIfFalse "Not the right" (redeemer ==  11)

------------------------------------------------------------------------------------------
-- Custom Types
------------------------------------------------------------------------------------------

newtype OurWonderfullDatum = OWD Integer
unstableMakeIsData ''OurWonderfullDatum

data OurWonderfullRedeemer = OWR Integer | JOKER Bool
makeIsDataIndexed ''OurWonderfullRedeemer [('OWR,0),('JOKER,1)]

newtype SwapDatum = Price Integer
unstableMakeIsData ''SwapDatum

-- data Action = Selling Integer | Cancel
-- unstableMakeIsData ''Action

data Action = Sell | Cancel 
unstableMakeIsData ''Action


{-# INLINABLE customTypedDatum22 #-}
customTypedDatum22 :: OurWonderfullDatum -> () -> ScriptContext -> Bool
customTypedDatum22 (OWD datum) _ _ = traceIfFalse "Not the right datum!" (datum ==  22)

{-# INLINABLE customTypedRedeemer11 #-}
customTypedRedeemer11 ::  () -> OurWonderfullRedeemer -> ScriptContext -> Bool
customTypedRedeemer11 _ (OWR number) _    =  traceIfFalse "Not the right redeemer!" (number ==  11)
customTypedRedeemer11 _ (JOKER boolean) _ =  traceIfFalse "The Joker sais no!" boolean

{-# INLINABLE swapDatum107 #-}
swapDatum107 :: SwapDatum -> Action -> ScriptContext -> Bool
swapDatum107 datum action ctx = case action of  
                                   Sell     -> traceIfFalse "Sell conditions not fullfilled!" sellConditions 
                                   Cancel   -> traceIfFalse "Cancelation conditions not fullfilled" cancelConditions 
 
    where   
        sellConditions :: Bool
        sellConditions = valueGreaterThanPrice && sellerGetsValue && buyerGetsNFT && swapSCgetsDaCut 

        cancelConditions :: Bool
        cancelConditions = True

        valueGreaterThanPrice :: SwapDatum -> Bool
        valueGreaterThanPrice (Price price) =  

        sellerGetsValue :: Bool
        sellerGetsValue = True

        buyerGetsNFT :: Bool
        buyerGetsNFT = True

        swapSCgetsDaCut :: Bool
        swapSCgetsDaCut = True

        info :: TxInfo
        info = scriptContextTxInfo ctx
       
        inputs :: [TxInInfo]  -- filter /x -> x =! (findOwnInput ctx)  
        inputs = filter txInfoInputs txinfo

        valueProvided :: [TxInInfo] -> Value
        valueProvided = foldMap (txOutValue . txInInfoResolved) 


------------------------------------------------------------------------------------------
-- Mappers and Compiling expresions
------------------------------------------------------------------------------------------

-- for primitives types

wrappedTypedDatum22 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedTypedDatum22 = wrapValidator customTypedDatum22

typedDatum22Val :: PlutusV2.Validator
typedDatum22Val = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedTypedDatum22 ||])

wrappedTypedRedeemer11 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedTypedRedeemer11 = wrapValidator typedRedeemer11

typedRedeemer11Val :: Validator
typedRedeemer11Val = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedTypedRedeemer11 ||])


-- for custom types

wrappedCustomTypedDatum22 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedCustomTypedDatum22 = wrapValidator customTypedDatum22

wrappedCustomTypedRedeemer11 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedCustomTypedRedeemer11 = wrapValidator customTypedRedeemer11

customTypedDatum22Val :: Validator
customTypedDatum22Val = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedCustomTypedDatum22 ||])

customTypedRedeemer11Val :: Validator
customTypedRedeemer11Val = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedCustomTypedRedeemer11 ||])

wrappedSwapDatum107 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedSwapDatum107 = wrapValidator swapDatum107

typedSwapDatum107 :: Validator
typedSwapDatum107 = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedSwapDatum107 ||])


------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

{- Serialised Scripts and Values -}

saveTypedDatum22 :: IO ()
saveTypedDatum22 =  writeValidatorToFile "./testnet/typedDatum22.uplc" typedDatum22Val

saveTypedRedeemer11 :: IO ()
saveTypedRedeemer11 =  writeValidatorToFile "./testnet/typedRedeemer11.plutus" typedRedeemer11Val

saveCustomTypedDatum22 :: IO ()
saveCustomTypedDatum22 =  writeValidatorToFile "./testnet/customTypedDatum22.plutus" customTypedDatum22Val

saveCustomTypedRedeemer11 :: IO ()
saveCustomTypedRedeemer11 =  writeValidatorToFile "./testnet/customTypedRedeemer11.plutus" customTypedRedeemer11Val

saveTypedSwap :: IO ()
saveTypedSwap =  writeValidatorToFile "./testnet/swapDatum107.uplc" typedSwapDatum107

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveValue11 :: IO ()
saveValue11 = writeDataToFile "./testnet/value11.json" (11 :: Integer)

saveValue22 :: IO ()
saveValue22 = writeDataToFile "./testnet/value22.json" (22 :: Integer)

saveGoodDatum  :: IO ()
saveGoodDatum = writeDataToFile "./testnet/goodOWD.json" (OWD 22)

saveBadDatum  :: IO ()
saveBadDatum = writeDataToFile "./testnet/badOWD.json" (OWD 23)

saveOWR  :: IO ()
saveOWR = writeDataToFile "./testnet/OWR.json" ()

saveGoodJOKER :: IO ()
saveGoodJOKER = writeDataToFile "./testnet/GoodJoker.json" (JOKER True)

saveBadJOKER :: IO ()
saveBadJOKER = writeDataToFile "./testnet/BadJoker.json" (JOKER False)

-- saveRedeemerSelling :: IO ()
-- saveRedeemerSelling = writeDataToFile "./testnet/selling50.json" (Selling 50)


saveAll :: IO ()
saveAll = do
            saveTypedDatum22
            saveTypedRedeemer11
            saveCustomTypedDatum22
            saveCustomTypedRedeemer11
            saveUnit
            saveValue11
            saveValue22
            saveGoodDatum
            saveBadDatum
            saveOWR
            saveGoodJOKER
            saveBadJOKER
            saveTypedSwap
            -- savePrice50
            -- saveRedeemerSelling