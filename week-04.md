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
