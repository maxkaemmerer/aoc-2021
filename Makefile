install:
	cabal update && cabal install --package-env=. --lib hspec hspec-contrib QuickCheck HUnit
test:
	runhaskell Spec.hs