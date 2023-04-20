{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# LANGUAGE ViewPatterns       #-}

module Plutus.Contracts.SM where

-- | Smart contract that allows two parties to exchange a fixed amount of ADA.
-- | The contract requires the first party to deposit the agreed amount of ADA
-- | before the second party can withdraw it.
-- | The contract can only be executed once.

-- | Import necessary modules
import PlutusTx
import PlutusTx.Prelude

import Data.ByteString.Lazy.Char8 qualified as C
import Ledger
-- import Ledger.Validation
import Ledger.Typed.Scripts qualified as Scripts
-- import Ledger.Value
import Data.String
import Plutus.Contract
import PlutusTx.Prelude
import Prelude qualified as Haskell


-- | Define the data type that represents the contract state
data ExchangeDatum = ExchangeDatum
    { expectedValue    :: Value
    , hasBeenExchanged :: Bool
    } deriving Haskell.Show

-- | Define the validator script that enforces the contract rules
-- | The validator requires the redeemer and datum to be of type 'ExchangeDatum'
mkValidator :: ExchangeDatum -> ExchangeDatum -> ScriptContext -> Bool
mkValidator old new ctx =
    traceIfFalse "Value not as expected" (expectedValue old == expectedValue new)
        && traceIfFalse "Contract already exchanged" (not $ hasBeenExchanged old)
        && traceIfFalse "Invalid signature" (txSignedBy (scriptContextTxInfo ctx) $ Address $ C.pack "address of the first party")

-- | Define the function that returns the expected value of the contract
getAmount :: ExchangeDatum -> Integer
getAmount dat = let (Ada x) = expectedValue dat in x

-- | Define the function that creates the initial datum for the contract
mkDatum :: Value -> ExchangeDatum
mkDatum val = ExchangeDatum
    { expectedValue = val
    , hasBeenExchanged = False
    }

-- | Define the function that creates the redeemer for the contract
mkRedeemer :: () -> ExchangeDatum -> Redeemer
mkRedeemer _ _ = Redeemer $ PlutusTx.toBuiltinData ()

-- | Define the endpoint that allows the first party to deposit the agreed amount of ADA
endpoint :: Contract () ExchangeDatum Text ()
endpoint = do
    val <- endpointValue
    let dat = mkDatum val
    let tx = mustPayToTheScript dat val
    void $ submitTxConstraints (Scripts.mustValidateIn (from now)) tx
    logInfo @String $ "Deposited " ++ show (getAmount dat) ++ " ADA."

-- | Define the main entry point for the contract
main :: Contract () ExchangeDatum Text ()
main = do
    pkh <- pubKeyHash <$> ownPubKey
    let addr = Scripts.validatorAddress $ Scripts.mkTypedValidator @ExchangeDatum @ExchangeDatum
            ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode (mkDatum $ Ada 10))
            $$(PlutusTx.compile [|| wrap ||])
        wrap = Scripts.wrapValidator @ExchangeDatum @ExchangeDatum
    tellInfo $ "Created contract at " ++ show addr
    logInfo @String $ "Expected value: " ++ show 10
    logInfo @String $ "Expected address: " ++ show addr
    logInfo @String $ "Expected signature: " ++ show pkh
    endpoint

{-# INLINABLE mkValidator #-}
{-# INLINABLE getAmount #-}
{-# INLINABLE mkDatum #-}
{-# INLINABLE mkRedeemer #-}
{-# INLINABLE endpoint #-}

