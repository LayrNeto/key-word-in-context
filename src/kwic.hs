module KWIC (
    KWICState,
    readFileIntoState,
    filterCharsInState,
    toDict,
    shift,
    srtd
) where

import System.IO
import Data.Char (toLower, isAlphaNum)
import Data.List (sort)
import qualified Data.Map as Map
import Control.Monad.State

type KWICState a = StateT [String] IO a

readFileIntoState :: FilePath -> KWICState ()
readFileIntoState path = do
    content <- liftIO $ readFile path
    put (lines content)

filterCharsInState :: KWICState ()
filterCharsInState = do
    phrases <- get
    let filtered = map filterChars phrases
    put filtered
  where
    filterChars = map (\c -> if isAlphaNum c || c == ' ' then c else ' ')

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
    let allShifts = [(unwords shift, "from " ++ phrase) | (phrase, shifts) <- Map.toList kwicMap, shift <- shifts]
     in sort allShifts