# Week 08
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
git checkout 2f11c28bd8f6d630daab582255e16d8408075bd7
nix-build \
  --option trusted-public-keys "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" \
  --option substituters https://hydra.iohk.io \
  -A plutus-playground.client -A plutus-playground.server
```
