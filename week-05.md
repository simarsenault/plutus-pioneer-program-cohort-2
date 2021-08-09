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
