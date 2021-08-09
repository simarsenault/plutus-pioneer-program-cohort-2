# Week 05
## Setup
### Update repositories
```
cd plutus-pioneer-program
git fetch
git pull origin main
```

In `code/week05/cabal.project`, change line `28` from `tag: 8f1a47674a99ac9bc2aba3231375d8d6de0641d2` to `tag: 8a20664f00d8f396920385947903761a9a897fe0`. (Source: [https://github.com/input-output-hk/plutus-pioneer-program/pull/36](https://github.com/input-output-hk/plutus-pioneer-program/pull/36))

```
cd ..
cd plutus
git checkout master
git fetch
git pull origin master
git checkout 8a20664f00d8f396920385947903761a9a897fe0
nix-build \
  --option trusted-public-keys "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" \
  --option substituters https://hydra.iohk.io \
  -A plutus-playground.client -A plutus-playground.server
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

  module Week05.Homework1 where

  import           Control.Monad              hiding (fmap)
  import           Control.Monad.Freer.Extras as Extras
  import           Data.Aeson                 (ToJSON, FromJSON)
  import           Data.Default               (Default (..))
  import           Data.Text                  (Text)
  import           Data.Void                  (Void)
  import           GHC.Generics               (Generic)
  import           Plutus.Contract            as Contract
  import           Plutus.Trace.Emulator      as Emulator
  import qualified PlutusTx
  import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
  import           Ledger                     hiding (mint, singleton)
  import           Ledger.Constraints         as Constraints
  import           Ledger.TimeSlot
  import qualified Ledger.Typed.Scripts       as Scripts
  import           Ledger.Value               as Value
  import           Playground.Contract        (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
  import           Playground.TH              (mkKnownCurrencies, mkSchemaDefinitions)
  import           Playground.Types           (KnownCurrency (..))
  import           Prelude                    (IO, Semigroup (..), Show (..), String, undefined)
  import           Text.Printf                (printf)
  import           Wallet.Emulator.Wallet

  {-# INLINABLE mkPolicy #-}
  -- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
  -- has signed the transaction and if the specified deadline has not passed.
  mkPolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
  mkPolicy pkh deadline () ctx = signedByPkh && deadlineIsValid
      where
          info :: TxInfo
          info = scriptContextTxInfo ctx

          signedByPkh :: Bool
          signedByPkh = txSignedBy (scriptContextTxInfo ctx) pkh

          deadlineIsValid :: Bool
          deadlineIsValid = contains (to deadline) (txInfoValidRange info)

  policy :: PubKeyHash -> POSIXTime -> Scripts.MintingPolicy
  policy pkh deadline = mkMintingPolicyScript $
      $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||])
      `PlutusTx.applyCode`
      (PlutusTx.liftCode pkh)
      `PlutusTx.applyCode`
      (PlutusTx.liftCode deadline)

  curSymbol :: PubKeyHash -> POSIXTime -> CurrencySymbol
  curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline

  data MintParams = MintParams
      { mpTokenName :: !TokenName
      , mpDeadline  :: !POSIXTime
      , mpAmount    :: !Integer
      } deriving (Generic, ToJSON, FromJSON, ToSchema)

  type SignedSchema = Endpoint "mint" MintParams

  mint :: MintParams -> Contract w SignedSchema Text ()
  mint mp = do
      pkh <- pubKeyHash <$> Contract.ownPubKey
      now <- Contract.currentTime
      let deadline = mpDeadline mp
      if now > deadline
          then Contract.logError @String "deadline passed"
          else do
              let val     = Value.singleton (curSymbol pkh deadline) (mpTokenName mp) (mpAmount mp)
                  lookups = Constraints.mintingPolicy $ policy pkh deadline
                  tx      = Constraints.mustMintValue val <> Constraints.mustValidateIn (to $ now + 5000)
              ledgerTx <- submitTxConstraintsWith @Void lookups tx
              void $ awaitTxConfirmed $ txId ledgerTx
              Contract.logInfo @String $ printf "forged %s" (show val)

  endpoints :: Contract () SignedSchema Text ()
  endpoints = mint' >> endpoints
    where
      mint' = endpoint @"mint" >>= mint

  mkSchemaDefinitions ''SignedSchema

  mkKnownCurrencies []

  test :: IO ()
  test = runEmulatorTraceIO $ do
      let tn       = "ABC"
          deadline = slotToBeginPOSIXTime def 10
      h <- activateContractWallet (Wallet 1) endpoints
      callEndpoint @"mint" h $ MintParams
          { mpTokenName = tn
          , mpDeadline  = deadline
          , mpAmount    = 555
          }
      void $ Emulator.waitNSlots 15
      callEndpoint @"mint" h $ MintParams
          { mpTokenName = tn
          , mpDeadline  = deadline
          , mpAmount    = 555
          }
      void $ Emulator.waitNSlots 1
</details>
