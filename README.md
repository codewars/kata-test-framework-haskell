# kata-test-framework-haskell

codewars.com kata test framework for the Haskell language

## Building and Running Tests

Make sure that you have `cabal` installed.

You will need to run `cabal` to install the following dependencies:
```bash
cabal install QuickCheck hspec aeson silently knob MissingH
cabal install json2 --ghc-options=-XFlexibleInstances
```

To build, run:

```bash
cabal configure --enable-tests
cabal build
cabal test
```
