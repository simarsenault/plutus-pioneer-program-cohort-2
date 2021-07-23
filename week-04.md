# Week 04
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
git checkout 2fbb7abb22138a434bb6c4f663a81e9b9dc51e98
nix-build \
  --option trusted-public-keys "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" \
  --option substituters https://hydra.iohk.io \
  -A plutus-playground.client -A plutus-playground.server
```

## Homework
<details>
    <summary>Solution</summary>
  
    {-# LANGUAGE DataKinds         #-}
    {-# LANGUAGE DeriveAnyClass    #-}
    {-# LANGUAGE DeriveGeneric     #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE TypeApplications  #-}
    {-# LANGUAGE TypeOperators     #-}

    module Week04.Homework where

    import Data.Aeson            (FromJSON, ToJSON)
    import Data.Functor          (void)
    import Data.Text             (Text, unpack)
    import GHC.Generics          (Generic)
    import Ledger
    import Ledger.Ada            as Ada
    import Ledger.Constraints    as Constraints
    import Plutus.Contract       as Contract
    import Plutus.Trace.Emulator as Emulator
    -- New imports needed
    import Wallet.Emulator.Wallet
    import Data.Void                  (Void)

    data PayParams = PayParams
        { ppRecipient :: PubKeyHash
        , ppLovelace  :: Integer
        } deriving (Show, Generic, FromJSON, ToJSON)

    type PaySchema = Endpoint "pay" PayParams

    payContract :: Contract () PaySchema Text ()
    payContract = do
        pp <- endpoint @"pay"
        let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
        void $ submitTx tx
        -- payContract -- Recursion will now be handled by payContract'

    -- New contract needed to catch errors from payContract
    payContract' :: Contract () PaySchema Void ()
    payContract' = do
        Contract.handleError (\err -> Contract.logError $ "caught: " ++ unpack err) payContract
        payContract'

    -- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
    -- recipient, but with amounts given by the two arguments. There should be a delay of one slot
    -- after each endpoint call.
    payTrace :: Integer -> Integer -> EmulatorTrace ()
    payTrace a1 a2 = do
        hw1 <- activateContractWallet (Wallet 1) payContract'
        callEndpoint @"pay" hw1 PayParams { ppRecipient = pubKeyHash (walletPubKey (Wallet 2)), ppLovelace = a1 }
        void $ Emulator.waitNSlots 1
        callEndpoint @"pay" hw1 PayParams { ppRecipient = pubKeyHash (walletPubKey (Wallet 2)), ppLovelace = a2 }
        void $ Emulator.waitNSlots 1

    payTest1 :: IO ()
    payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

    payTest2 :: IO ()
    payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000  
</details>
