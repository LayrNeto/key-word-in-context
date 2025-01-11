{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import KWIC
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd, FileInfo(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Char (toLower)
import Control.Monad.State
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (badRequest400)

-- Auxiliary data type to return a list formatted in JSON
newtype ResultData = ResultData { results :: [String] } deriving (Show, Generic)

instance ToJSON ResultData

main :: IO ()
main = do
    -- Reads the default stop words file
    stopWordsContent <- readFile "data/stop_words.txt"
    let defaultStopWords = map (map toLower) $ words stopWordsContent

    Scotty.scotty 3000 $ do
        -- Middleware for request logging and static files
        Scotty.middleware logStdoutDev
        Scotty.middleware $ staticPolicy (addBase "static")

        -- Main route to display the form
        Scotty.get "/" $ Scotty.file "static/index.html"

        -- Route to process the file upload
        Scotty.post "/process" $ do
            -- Gets the request
            req <- Scotty.request

            -- Parses the request body
            (_, files) <- Scotty.liftIO $ parseRequestBody lbsBackEnd req

            -- Checks if the main file was uploaded
            case lookup "file" files of
                Just fileInfo -> do
                    let content = BL.toStrict $ fileContent fileInfo
                    let inputText = decodeUtf8 $ BL.fromStrict content
                    Scotty.liftIO $ putStrLn $ "File received:\n" ++ T.unpack inputText

                    -- Splits the content into lines
                    let phrases = lines $ T.unpack inputText

                    -- Checks if the stop words file was uploaded
                    let stopWordsFile = lookup "stopWords" files
                    stopWords <- case stopWordsFile of
                        Just swFile -> do
                            -- Reads the stop words file provided by the user
                            let swContent = BL.toStrict $ fileContent swFile
                            return $ map (map toLower) $ words $ T.unpack $ decodeUtf8 $ BL.fromStrict swContent
                        Nothing -> return defaultStopWords -- Uses the default if no file is provided

                    Scotty.liftIO $ putStrLn $ "Stop words in use: " ++ unwords stopWords

                    -- Processes the phrases
                    result <- Scotty.liftIO $ processKWIC stopWords phrases
                    Scotty.liftIO $ putStrLn $ "Result sent:\n" ++ unlines result

                    -- Returns the results in JSON
                    Scotty.json $ ResultData result

                Nothing -> do
                    Scotty.status badRequest400
                    Scotty.text "Error: No file found in the upload"

-- Function that processes the phrases using the KWIC pipeline
processKWIC :: [String] -> [String] -> IO [String]
processKWIC stopWords inputPhrases =
    execStateT (put inputPhrases >> filterCharsInState) [] >>= \filteredPhrases ->
        evalStateT (toDict >>= \dict -> return (map formatResult (srtd (shift stopWords dict)))) filteredPhrases

-- Formats the results in the style "shifted (source)"
formatResult :: (String, String) -> String
formatResult (shifted, source) = shifted ++ " (" ++ source ++ ")"
