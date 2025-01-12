module KWIC (
    KWICState,
    filterCharsInState,
    toDict,
    shift,
    srtd,
    formatResult,
    processKWIC
) where

import System.IO()
import Data.Char (toLower, isAlphaNum)
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as Map
import Control.Monad.State

type KWICState a = StateT [String] IO a

filterCharsInState :: KWICState ()
filterCharsInState = do
    phrases <- get
    let filtered = map filterChars phrases
    put filtered
  where
    filterChars = unwords . words . map (\c -> if isAlphaNum c || c == ' ' then c else ' ')


toDict :: KWICState (Map.Map String [[String]])
toDict = do
    phrases <- get
    let dict = Map.fromList [(phrase, []) | phrase <- phrases]
    return dict

shift :: [String] -> Map.Map String [[String]] -> Map.Map String [[String]]
shift stopWords = Map.mapWithKey generateShifts
  where
    isStopWord word = map toLower word `elem` stopWords

    generateShifts phrase _ =
        let wordsList = words phrase
         in [rotate i wordsList | i <- [0 .. length wordsList - 1], not (isStopWord (wordsList !! i))]
    rotate i xs = drop i xs ++ take i xs

srtd :: Map.Map String [[String]] -> [(String, String)]
srtd kwicMap =
    let allShifts = [(unwords shiftedPhrase, "from " ++ phrase) | (phrase, shifts) <- Map.toList kwicMap, shiftedPhrase <- shifts]
    in sortBy (compare `on` (map toLower . fst)) allShifts

formatResult :: (String, String) -> String
formatResult (shifted, source) = shifted ++ " (" ++ source ++ ")"

processKWIC :: [String] -> [String] -> IO [String]
processKWIC stopWords inputPhrases =
    execStateT (put inputPhrases >> filterCharsInState) [] >>= \filteredPhrases ->
        evalStateT (toDict >>= \dict -> return (map formatResult (srtd (shift stopWords dict)))) filteredPhrases