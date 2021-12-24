### Setup
1. Install hspec dependencies `cabal update && cabal install --package-env=. --lib hspec hspec-contrib QuickCheck HUnit`

### Run Tests (https://hspec.github.io/)
1. execute `runhaskell Spec.hs`

### Notes
1. Before you implement a function go to [hoogle](https://hoogle.haskell.org/) and check for the signature. `[Int] -> [[Int]]` for example, which is just `tails`
2. ``transpose`` is basically ``zip`` on steroids, given the list of lists ``[[1,2,3,4],[1,2,3,4],[1,2,3,4]]`` you get ``[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]``