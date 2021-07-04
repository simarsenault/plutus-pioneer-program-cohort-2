## Week 01 
### Setup
#### Hardware Specifications
##### Host Machine
- Windows 10 x64
- Intel i7-2600k (stock)
- 16 GB RAM

##### Guest Machine
- Ubuntu 20.04 LTS x64 (desktop with minimal install)
- 4 vCore allocated
- 8GB RAM allocated

1. Install Ubuntu on the guest machine
2. Install updates: `sudo apt update`, `sudo apt upgrade`
3. Install dependencies: `sudo apt install curl rsync git`
4. Install nix
    1. `sh <(curl -L https://nixos.org/nix/install) --daemon`
    2. Follow prompts (allow sudo)
    3. Configure cache:
        ```
        sudo sh -c "echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> /etc/nix/nix.conf"
        sudo sh -c "echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> /etc/nix/nix.conf"
        sudo systemctl restart nix-daemon.service
        ```
    4. Logout & relog
    5. Validate: `nix-env --version` should return `nix-env (Nix) 2.3.10` (version may be different)
5. Build nix shell for Plutus playground:
    ```
    git clone https://github.com/input-output-hk/plutus
    cd plutus
    git checkout ea0ca4e9f9821a9dbfc5255fa0f42b6f2b3887c4
    nix-build \
      --option trusted-public-keys "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" \
      --option substituters https://hydra.iohk.io \
      -A plutus-playground.client -A plutus-playground.server
    ```
6. Launch Plutus Playground Server (must be inside `plutus` folder from step 5):
    ```
    nix-shell
    cd plutus-playground-server
    plutus-playground-server
    ```
7. Launch Plutus Playground Client (must be inside `plutus` folder from step 5):
    ```
    nix-shell
    cd plutus-playground-client
    rm -rf .spago
    npm run start
    ``` 
8. Open browser at https://localhost:8009 (accept self signed certificate)
9. Enjoy :)

##### Unresolved problems
1. Sometimes, the `Evaluate` button of the auction script produces an error. To fix, simply try again.
    ```
    ConnectionError (HttpExceptionRequest Request { host = "localhost" port = 8080 secure = False requestHeaders = [("Accept","application/json;charset=utf-8,application/json"),("Content-Type","application/json;charset=utf-8")] path = "/runghc" queryString = "" method = "POST" proxy = Nothing rawBody = False redirectCount = 10 responseTimeout = ResponseTimeoutDefault requestVersion = HTTP/1.1 } ResponseTimeout)
    ```
    There seems to be a timeout at exactly 30 seconds and I don't know how to increase it. 

##### Sources
1. Nix installation: https://docs.plutus-community.com/docs/setup/Ubuntu.html
2. Git hash for week 01: https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week01/cabal.project
3. Nix shell build command: https://github.com/input-output-hk/plutus/tree/master/plutus-playground-client
4. `rm -rf .spago` to fix `DuplicateModule` error: @toky03#4368 on Discord
