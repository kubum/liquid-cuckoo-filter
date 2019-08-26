# Cuckoo Filter

An implementation of [Cuckoo Filter](https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf) with LiquidHaskell bindings. 

## Build

```sh
$ stack build
```

## Tests

```sh
$ stack test
```

## LiquidHaskell

Please use `liquid` from the `stack` environment and give it the file to check
```sh
$ stack exec -- liquid src/Data/LiquidCuckooFilterPlain.hs
```