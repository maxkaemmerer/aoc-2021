-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Solutions.Day01 as Day01
import qualified Solutions.Day02 as Day02
import qualified Solutions.Day03 as Day03
import qualified Solutions.Day04 as Day04

main :: IO ()
main = hspec $ do
  describe "day-01" $ do
    it "part 1 example should be 7" $ do
      result <- Day01.mainPartOne "./data/day-01/example.txt"
      result `shouldBe` (7 :: Int)

    it "part 1 actual should be 1475" $ do
      result <- Day01.mainPartOne "./data/day-01/measurements.txt"
      result `shouldBe` (1475 :: Int)
    
    it "part 2 example should be 5" $ do
      result <- Day01.mainPartTwo "./data/day-01/example.txt"
      result `shouldBe` (5 :: Int)    
    
    it "part 2 actual should be 1516" $ do
      result <- Day01.mainPartTwo "./data/day-01/measurements.txt"
      result `shouldBe` (1516 :: Int)
  
  describe "day-02" $ do
    it "part 1 example should be 150" $ do
      result <- Day02.mainPartOne "./data/day-02/example.txt"
      result `shouldBe` (150 :: Int)

    it "part 1 actual should be 1484118" $ do
      result <- Day02.mainPartOne "./data/day-02/instructions.txt"
      result `shouldBe` (1484118 :: Int)
    
    it "part 2 example should be 900" $ do
      result <- Day02.mainPartTwo "./data/day-02/example.txt"
      result `shouldBe` (900 :: Int)    
    
    it "part 2 actual should be 1463827010" $ do
      result <- Day02.mainPartTwo "./data/day-02/instructions.txt"
      result `shouldBe` (1463827010 :: Int)

  describe "day-03" $ do
    it "part 1 example should be 198" $ do
      result <- Day03.mainPartOne "./data/day-03/example.txt"
      result `shouldBe` (198 :: Int)

    it "part 1 actual should be 2261546" $ do
      result <- Day03.mainPartOne "./data/day-03/diagnostics.txt"
      result `shouldBe` (2261546 :: Int)
    
    it "part 2 example should be 230" $ do
      result <- Day03.mainPartTwo "./data/day-03/example.txt"
      result `shouldBe` (230 :: Int)    
    
    it "part 2 actual should be 6775520" $ do
      result <- Day03.mainPartTwo "./data/day-03/diagnostics.txt"
      result `shouldBe` (6775520 :: Int)

  describe "day-04" $ do
    it "part 1 example should be 4512" $ do
      result <- Day04.mainPartOne "./data/day-04/example.txt"
      result `shouldBe` (4512 :: Int)

    it "part 1 actual should be 10374" $ do
      result <- Day04.mainPartOne "./data/day-04/boards.txt"
      result `shouldBe` (10374 :: Int)
    
    it "part 2 example should be 1924" $ do
      result <- Day04.mainPartTwo "./data/day-04/example.txt"
      result `shouldBe` (1924 :: Int)    
    
    it "part 2 actual should be 24742" $ do
      result <- Day04.mainPartTwo "./data/day-04/boards.txt"
      result `shouldBe` (24742 :: Int)