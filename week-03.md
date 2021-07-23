# Week 03
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
git checkout 219992289c6615e197069d022735cb4059d43229
nix-store --gc
nix-build \
  --option trusted-public-keys "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" \
  --option substituters https://hydra.iohk.io \
  -A plutus-playground.client -A plutus-playground.server
```

### New in plutus-playground-server
1. The server can now be started with `-i` argument to increase timeout of compilation/simulation
    ```
    plutus-playground-server -i 120s
    ```

## Useful Commands
1. Get public key hash of wallet:
    ```
    import Ledger
    import Wallet.Emulator
    pubKeyHash (walletPubKey (Wallet 2))
    ```
2. Get POSIX time:
    ```
    import Ledger.TimeSlot
    import Data.Default
    slotToBeginPOSIXTime def 10
    ```

## Homework 1
<details>
    <summary>Solution</summary>
  
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

    {-# OPTIONS_GHC -fno-warn-unused-imports #-}

    module Week03.Homework1 where

    import           Control.Monad        hiding (fmap)
    import           Data.Aeson           (ToJSON, FromJSON)
    import           Data.Map             as Map
    import           Data.Text            (Text)
    import           Data.Void            (Void)
    import           GHC.Generics         (Generic)
    import           Plutus.Contract
    import qualified PlutusTx
    import           PlutusTx.Prelude     hiding (unless)
    import           Ledger               hiding (singleton)
    import           Ledger.Constraints   as Constraints
    import qualified Ledger.Typed.Scripts as Scripts
    import           Ledger.Ada           as Ada
    import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
    import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
    import           Playground.Types     (KnownCurrency (..))
    import           Prelude              (IO)
    import qualified Prelude              as P
    import           Text.Printf          (printf)

    data VestingDatum = VestingDatum
        { beneficiary1 :: PubKeyHash  -- Wallet receiving
        , beneficiary2 :: PubKeyHash  -- Wallet giving
        , deadline     :: POSIXTime
        } deriving P.Show

    PlutusTx.unstableMakeIsData ''VestingDatum

    {-# INLINABLE mkValidator #-}
    -- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
    -- or if beneficiary2 has signed the transaction and the deadline has passed.
    mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
    mkValidator dat () ctx = beneficiary1CanGrab || beneficiary2CanGrab
      where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        beneficiary1CanGrab :: Bool
        beneficiary1CanGrab = txSignedBy info (beneficiary1 dat) && contains (to (deadline dat)) (txInfoValidRange info)

        beneficiary2CanGrab :: Bool
        beneficiary2CanGrab = txSignedBy info (beneficiary2 dat) && contains (from (1 + deadline dat)) (txInfoValidRange info)

    data Vesting
    instance Scripts.ValidatorTypes Vesting where
        type instance DatumType Vesting = VestingDatum
        type instance RedeemerType Vesting = ()

    typedValidator :: Scripts.TypedValidator Vesting
    typedValidator = Scripts.mkTypedValidator @Vesting
        $$(PlutusTx.compile [|| mkValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @VestingDatum @()

    validator :: Validator
    validator = Scripts.validatorScript typedValidator

    valHash :: Ledger.ValidatorHash
    valHash = Scripts.validatorHash typedValidator

    scrAddress :: Ledger.Address
    scrAddress = scriptAddress validator

    data GiveParams = GiveParams
        { gpBeneficiary :: !PubKeyHash
        , gpDeadline    :: !POSIXTime
        , gpAmount      :: !Integer
        } deriving (Generic, ToJSON, FromJSON, ToSchema)

    type VestingSchema =
                Endpoint "give" GiveParams
            .\/ Endpoint "grab" ()

    give :: AsContractError e => GiveParams -> Contract w s e ()
    give gp = do
        pkh <- pubKeyHash <$> ownPubKey
        let dat = VestingDatum
                    { beneficiary1 = gpBeneficiary gp
                    , beneficiary2 = pkh
                    , deadline     = gpDeadline gp
                    }
            tx  = mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
        ledgerTx <- submitTxConstraints typedValidator tx
        void $ awaitTxConfirmed $ txId ledgerTx
        logInfo @P.String $ printf "made a gift of %d lovelace to %s with deadline %s"
            (gpAmount gp)
            (P.show $ gpBeneficiary gp)
            (P.show $ gpDeadline gp)

    grab :: forall w s e. AsContractError e => Contract w s e ()
    grab = do
        now    <- currentTime
        pkh    <- pubKeyHash <$> ownPubKey
        utxos  <- utxoAt scrAddress
        let utxos1 = Map.filter (isSuitable $ \dat -> beneficiary1 dat == pkh && now <= deadline dat) utxos
            utxos2 = Map.filter (isSuitable $ \dat -> beneficiary2 dat == pkh && now >  deadline dat) utxos
        logInfo @P.String $ printf "found %d gift(s) to grab" (Map.size utxos1 P.+ Map.size utxos2)
        unless (Map.null utxos1) $ do
            let orefs   = fst <$> Map.toList utxos1
                lookups = Constraints.unspentOutputs utxos1 P.<>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] P.<>
                          mustValidateIn (to now)
            void $ submitTxConstraintsWith @Void lookups tx
        unless (Map.null utxos2) $ do
            let orefs   = fst <$> Map.toList utxos2
                lookups = Constraints.unspentOutputs utxos2 P.<>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] P.<>
                          mustValidateIn (from now)
            void $ submitTxConstraintsWith @Void lookups tx
      where
        isSuitable :: (VestingDatum -> Bool) -> TxOutTx -> Bool
        isSuitable p o = case txOutDatumHash $ txOutTxOut o of
            Nothing -> False
            Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
                Nothing        -> False
                Just (Datum e) -> maybe False p $ PlutusTx.fromData e

    endpoints :: Contract () VestingSchema Text ()
    endpoints = (give' `select` grab') >> endpoints
      where
        give' = endpoint @"give" >>= give
        grab' = endpoint @"grab" >>  grab

    mkSchemaDefinitions ''VestingSchema

    mkKnownCurrencies []
</details>
  
## Homework 2
<details>
    <summary>Solution</summary>
  
    {-# LANGUAGE DataKinds             #-}
    {-# LANGUAGE DeriveAnyClass        #-}
    {-# LANGUAGE DeriveGeneric         #-}
    {-# LANGUAGE FlexibleContexts      #-}
    {-# LANGUAGE MultiParamTypeClasses #-}
    {-# LANGUAGE NoImplicitPrelude     #-}
    {-# LANGUAGE OverloadedStrings     #-}
    {-# LANGUAGE ScopedTypeVariables   #-}
    {-# LANGUAGE TemplateHaskell       #-}
    {-# LANGUAGE TypeApplications      #-}
    {-# LANGUAGE TypeFamilies          #-}
    {-# LANGUAGE TypeOperators         #-}

    module Week03.Homework2 where

    import           Control.Monad        hiding (fmap)
    import           Data.Aeson           (ToJSON, FromJSON, Value (Bool))
    import           Data.Map             as Map
    import           Data.Text            (Text)
    import           Data.Void            (Void)
    import           GHC.Generics         (Generic)
    import           Plutus.Contract
    import qualified PlutusTx
    import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
    import           Ledger               hiding (singleton)
    import           Ledger.Constraints   as Constraints
    import qualified Ledger.Typed.Scripts as Scripts
    import           Ledger.Ada           as Ada
    import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
    import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
    import           Playground.Types     (KnownCurrency (..))
    import           Prelude              (IO, Semigroup (..), Show (..), String, undefined)
    import           Text.Printf          (printf)

    {-# OPTIONS_GHC -fno-warn-unused-imports #-}

    {-# INLINABLE mkValidator #-}
    mkValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
    mkValidator beneficiaryPkh deadline () ctx = traceIfFalse "beneficiary's signature missing" (txSignedBy info beneficiaryPkh) && 
                                                 traceIfFalse "deadline not reached" (contains (from deadline) (txInfoValidRange info))
      where
        info :: TxInfo
        info = scriptContextTxInfo ctx

    data Vesting
    instance Scripts.ValidatorTypes Vesting where
        type instance DatumType Vesting = POSIXTime
        type instance RedeemerType Vesting = ()

    typedValidator :: PubKeyHash -> Scripts.TypedValidator Vesting
    typedValidator p = Scripts.mkTypedValidator @Vesting
        ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
        $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @POSIXTime @()

    validator :: PubKeyHash -> Validator
    validator = Scripts.validatorScript . typedValidator

    scrAddress :: PubKeyHash -> Ledger.Address
    scrAddress = scriptAddress . validator

    data GiveParams = GiveParams
        { gpBeneficiary :: !PubKeyHash
        , gpDeadline    :: !POSIXTime
        , gpAmount      :: !Integer
        } deriving (Generic, ToJSON, FromJSON, ToSchema)

    type VestingSchema =
                Endpoint "give" GiveParams
            .\/ Endpoint "grab" ()

    give :: AsContractError e => GiveParams -> Contract w s e ()
    give gp = do
        let p  = gpBeneficiary gp
            d  = gpDeadline gp
            tx = mustPayToTheScript d $ Ada.lovelaceValueOf $ gpAmount gp
        ledgerTx <- submitTxConstraints (typedValidator p) tx
        void $ awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
            (gpAmount gp)
            (show $ gpBeneficiary gp)
            (show $ gpDeadline gp)

    grab :: forall w s e. AsContractError e => Contract w s e ()
    grab = do
        now   <- currentTime
        pkh   <- pubKeyHash <$> ownPubKey
        utxos <- Map.filter (isSuitable now) <$> utxoAt (scrAddress pkh)
        if Map.null utxos
            then logInfo @String $ "no gifts available"
            else do
                let orefs   = fst <$> Map.toList utxos
                    lookups = Constraints.unspentOutputs utxos        <>
                              Constraints.otherScript (validator pkh)
                    tx :: TxConstraints Void Void
                    tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                              mustValidateIn (from now)
                ledgerTx <- submitTxConstraintsWith @Void lookups tx
                void $ awaitTxConfirmed $ txId ledgerTx
                logInfo @String $ "collected gifts"
      where
        isSuitable :: POSIXTime -> TxOutTx -> Bool
        isSuitable now o = case txOutDatumHash $ txOutTxOut o of
            Nothing -> False
            Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
                Nothing        -> False
                Just (Datum e) -> case PlutusTx.fromData e of
                    Nothing -> False
                    Just d  -> d <= now

    endpoints :: Contract () VestingSchema Text ()
    endpoints = (give' `select` grab') >> endpoints
      where
        give' = endpoint @"give" >>= give
        grab' = endpoint @"grab" >>  grab

    mkSchemaDefinitions ''VestingSchema

    mkKnownCurrencies []  
</details>
