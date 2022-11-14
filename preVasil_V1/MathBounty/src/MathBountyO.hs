{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}

module MathBountyO where

import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>), Semigroup (..))

import           Ledger                    
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada                as Ada
import           Ledger.TimeSlot

--ON-CHAIN CODE

data MathBountyDatum = MBD 
                      { mbdMath :: Integer
                      , mbdDeadline :: POSIXTime
                      }
                      
PlutusTx.makeIsDataIndexed ''MathBountyDatum [('MBD,0)]

{-# INLINABLE mathBountyValidator #-}
mathBountyValidator :: MathBountyDatum -> Integer -> ScriptContext -> Bool 
mathBountyValidator datum redeemer sContext = traceIfFalse "Wrong X!" (mbdMath datum == redeemer*redeemer) &&
                                              traceIfFalse "Deadline passed" deadlineReached
    where
      info :: TxInfo
      info = scriptContextTxInfo sContext

      deadlineReached :: Bool
      deadlineReached =  (to $ mbdDeadline datum) `contains` (txInfoValidRange info)

data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = MathBountyDatum

bountyValidator :: Scripts.TypedValidator MathBounty
bountyValidator = Scripts.mkTypedValidator @MathBounty
    $$(PlutusTx.compile [|| mathBountyValidator ||])
    $$(PlutusTx.compile [|| wrapping ||])
     where
       wrapping  = Scripts.wrapValidator @MathBountyDatum @Integer

validator :: Validator
validator = Scripts.validatorScript bountyValidator

bountyAddress :: Ledger.Address       
bountyAddress = scriptAddress validator  