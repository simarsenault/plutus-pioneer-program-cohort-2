# Week 02
## Setup
### Update repositories
```
cd plutus-pioneer-program
git fetch
git pull origin main

cd ..
cd plutus
git checkout master
git fetch
git pull origin master
git checkout 81ba78edb1d634a13371397d8c8b19829345ce0d
nix-store --gc
nix-build \
  --option trusted-public-keys "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" \
  --option substituters https://hydra.iohk.io \
  -A plutus-playground.client -A plutus-playground.server
```

## Homework 1
<details>
    <summary>Solution</summary>

    ```
    {-# LANGUAGE DataKinds           #-}
    {-# LANGUAGE FlexibleContexts    #-}
    {-# LANGUAGE NoImplicitPrelude   #-}
    {-# LANGUAGE OverloadedStrings   #-}
    {-# LANGUAGE ScopedTypeVariables #-}
    {-# LANGUAGE TemplateHaskell     #-}
    {-# LANGUAGE TypeApplications    #-}
    {-# LANGUAGE TypeFamilies        #-}
    {-# LANGUAGE TypeOperators       #-}

    {-# OPTIONS_GHC -fno-warn-unused-imports #-}

    module Week02.Homework1 where

    import           Control.Monad        hiding (fmap)
    import           Data.Map             as Map
    import           Data.Text            (Text)
    import           Data.Void            (Void)
    import           Plutus.Contract
    import qualified PlutusTx
    import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
    import           Ledger               hiding (singleton)
    import           Ledger.Constraints   as Constraints
    import qualified Ledger.Typed.Scripts as Scripts
    import           Ledger.Ada           as Ada
    import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
    import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
    import           Playground.Types     (KnownCurrency (..))
    import           Prelude              (IO, Semigroup (..), String, undefined)
    import           Text.Printf          (printf)

    {-# INLINABLE mkValidator #-}
    -- This should validate if and only if the two Booleans in the redeemer are equal!
    mkValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
    mkValidator _ (f, s) _ = traceIfFalse "pair not equal" (f == s)

    data Typed
    instance Scripts.ValidatorTypes Typed where
        type instance DatumType Typed = ()
        type instance RedeemerType Typed = (Bool, Bool)

    typedValidator :: Scripts.TypedValidator Typed
    typedValidator = Scripts.mkTypedValidator @Typed
        $$(PlutusTx.compile [|| mkValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @(Bool, Bool)

    validator :: Validator
    validator = Scripts.validatorScript typedValidator

    valHash :: Ledger.ValidatorHash
    valHash = Scripts.validatorHash typedValidator

    scrAddress :: Ledger.Address
    scrAddress = scriptAddress validator

    type GiftSchema =
                Endpoint "give" Integer
            .\/ Endpoint "grab" (Bool, Bool)

    give :: AsContractError e => Integer -> Contract w s e ()
    give amount = do
        let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
        ledgerTx <- submitTxConstraints typedValidator tx
        void $ awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ printf "made a gift of %d lovelace" amount

    grab :: forall w s e. AsContractError e => (Bool, Bool) -> Contract w s e ()
    grab bs = do
        utxos <- utxoAt scrAddress
        let orefs   = fst <$> Map.toList utxos
            lookups = Constraints.unspentOutputs utxos      <>
                    Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData bs | oref <- orefs]
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ "collected gifts"

    endpoints :: Contract () GiftSchema Text ()
    endpoints = (give' `select` grab') >> endpoints
    where
        give' = endpoint @"give" >>= give
        grab' = endpoint @"grab" >>= grab

    mkSchemaDefinitions ''GiftSchema

    mkKnownCurrencies []
    ```
</details>