module CoreSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Control.Monad.State (execStateT, evalStateT)
import KWIC

spec :: Spec  
spec = describe "Core Functions" $ do
    describe "filterCharsInState" $ do
        it "removes non-alphanumeric characters from phrases" $ do
            let input = ["Hello, World!"]
                expected = ["Hello World"]
            execStateT filterCharsInState input `shouldReturn` expected

    describe "toDict" $ do
        it "creates a dictionary from phrases" $ do
            let input = ["The quick brown fox"]
                expected = Map.fromList [("The quick brown fox", [])]
            evalStateT toDict input `shouldReturn` expected

    describe "shift" $ do
        it "shifts phrases excluding stop words" $ do
            let stopWords = ["the", "a", "is", "of", "on", "and"]
            let inputDict = Map.fromList [("The quick brown fox", [])]
                expectedOutput = Map.fromList [("The quick brown fox", [["quick", "brown", "fox", "The"],
                                                                        ["brown", "fox", "The", "quick"],
                                                                        ["fox", "The", "quick", "brown"]])]
            shift stopWords inputDict `shouldBe` expectedOutput

    describe "srtd" $ do
        it "sorts shifted phrases alphabetically" $ do
            let inputDict = Map.fromList [("The quick brown fox", [["quick", "brown", "fox", "The"],
                                                                   ["brown", "fox", "The", "quick"],
                                                                   ["fox", "The", "quick", "brown"]]),
                                          ("A brown cat sat", [["brown", "cat", "sat", "A"],
                                                               ["cat", "sat", "A", "brown"],
                                                               ["sat", "A", "brown", "cat"]])                        ]
                expectedOutput = [("brown cat sat A", "from A brown cat sat"),
                                  ("brown fox The quick", "from The quick brown fox"),
                                  ("cat sat A brown", "from A brown cat sat"),
                                  ("fox The quick brown", "from The quick brown fox"),
                                  ("quick brown fox The", "from The quick brown fox"),
                                  ("sat A brown cat", "from A brown cat sat")]
            srtd inputDict `shouldBe` expectedOutput
