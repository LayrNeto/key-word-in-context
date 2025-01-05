module IntegrationSpec (spec) where

import Test.Hspec
import KWIC
import Data.Map()
import Control.Monad.State (execStateT, evalStateT, put)

spec :: Spec
spec = describe "KWIC Integration" $ do
    it "processes the entire pipeline correctly with a single phrase" $ do
        let stopWords = ["the", "a", "is", "of", "on", "and"]
        let inputPhrases = ["The quick brown fox"]
        let expected = [("brown fox The quick", "from The quick brown fox"),
                        ("fox The quick brown", "from The quick brown fox"),
                        ("quick brown fox The", "from The quick brown fox")]
        
        result <- execStateT (put inputPhrases >> filterCharsInState) [] >>= \filteredPhrases ->
                  evalStateT (toDict >>= \dict -> return (srtd (shift stopWords dict))) filteredPhrases
        result `shouldBe` expected

    it "processes the entire pipeline correctly with multiple phrases" $ do
        let stopWords = ["the", "a", "is", "of", "on", "and"]
        let inputPhrases = ["The quick brown fox", "The cat is brown", "A brown cat sat"]
        let expected = [("brown cat sat A", "from A brown cat sat"),
                        ("brown fox The quick", "from The quick brown fox"),
                        ("brown The cat is", "from The cat is brown"),
                        ("cat is brown The", "from The cat is brown"),
                        ("cat sat A brown", "from A brown cat sat"),
                        ("fox The quick brown", "from The quick brown fox"),
                        ("quick brown fox The", "from The quick brown fox"),
                        ("sat A brown cat", "from A brown cat sat")]

        result <- execStateT (put inputPhrases >> filterCharsInState) [] >>= \filteredPhrases ->
                  evalStateT (toDict >>= \dict -> return (srtd (shift stopWords dict))) filteredPhrases
        result `shouldBe` expected

    it "processes the entire pipeline correctly with an empty phrase" $ do
        let stopWords = ["the", "a", "is", "of", "on", "and"]
        let inputPhrases = [""]
        let expected = []

        result <- execStateT (put inputPhrases >> filterCharsInState) [] >>= \filteredPhrases ->
                  evalStateT (toDict >>= \dict -> return (srtd (shift stopWords dict))) filteredPhrases
        result `shouldBe` expected
