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
