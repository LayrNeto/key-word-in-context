module Main (main) where
  
import KWIC
import System.IO()
import Data.Char (toLower)
import Control.Monad.State
import Data.Map()

main :: IO ()
main = do
    stopWordsContent <- readFile "stop_words.txt"
    let stopWords = map (map toLower) $ words stopWordsContent

    result <- execStateT (readFileIntoState "input_phrases.txt" >> filterCharsInState) [] >>= \filteredPhrases ->
              evalStateT (toDict >>= \dict -> (return . srtd) (shift stopWords dict)) filteredPhrases

    mapM_ printResult result
  where
    printResult (shifted, source) = putStrLn $ shifted ++ " (" ++ source ++ ")"
