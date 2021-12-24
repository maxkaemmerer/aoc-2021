## Introduction
In this project I am slowly making by way through [Advent Of Code 2021](https://adventofcode.com/2021) using Haskell, a language I always wanted to learn but always struggled with. The code in this repo is not optimized and can most definetly be improved. This is simply a way for me to learn a new language and not a recommendation on how to write or structure your Haskell code.

### Setup
1. Install hspec dependencies `cabal update && cabal install --package-env=. --lib hspec hspec-contrib QuickCheck HUnit`

### Run Tests (https://hspec.github.io/)
1. execute `runhaskell Spec.hs`

### Notes
1. Before you implement a function go to [hoogle](https://hoogle.haskell.org/) and check for the signature. `[Int] -> [[Int]]` for example, which is just `tails`
2. ``transpose`` is basically ``zip`` on steroids, given the list of lists ``[[1,2,3,4],[1,2,3,4],[1,2,3,4]]`` you get ``[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]``