# plutus-pioneer-program-cohort-2
## Notes
Personal notes from the Plutus Pioneer Program (2nd cohort)

1. [Week 01 - Introduction, Setup and Playground](/week-01.md)
2. [Week 02 - On-chain validation scripts](/week-02.md)
3. [Week 03 - Script contexts, parameterized contracts](/week-03.md)
4. [Week 04 - Monads, `Emulator`, `Contract`](/week-04.md)

## Cheat Sheet
1. Get public key hash of wallet ([week 03](week-03.md#useful-commands)):
    ```
    import Ledger
    import Wallet.Emulator
    pubKeyHash (walletPubKey (Wallet 2))
    ```
2. Get POSIX time ([week 03](week-03.md#useful-commands)):
    ```
    import Ledger.TimeSlot
    import Data.Default
    slotToBeginPOSIXTime def 10
    ```
