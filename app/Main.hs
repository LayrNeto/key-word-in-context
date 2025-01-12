{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import KWIC (processKWIC)
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd, FileInfo(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Char (toLower)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (badRequest400)

newtype ResultData = ResultData { results :: [String] } deriving (Show, Generic)

instance ToJSON ResultData

main :: IO ()
main = do
    stopWordsContent <- readFile "data/stop_words.txt"
    let defaultStopWords = map (map toLower) $ words stopWordsContent

    Scotty.scotty 3000 $ do
        Scotty.middleware logStdoutDev
        Scotty.middleware $ staticPolicy (addBase "static")

        Scotty.get "/" $ Scotty.file "static/index.html"

        Scotty.post "/process" $ do
            req <- Scotty.request

            (_, files) <- Scotty.liftIO $ parseRequestBody lbsBackEnd req

            case lookup "file" files of
                Just fileInfo -> do
                    let content = BL.toStrict $ fileContent fileInfo
                    let inputText = decodeUtf8 $ BL.fromStrict content
                    Scotty.liftIO $ putStrLn $ "File received:\n" ++ T.unpack inputText

                    let phrases = lines $ T.unpack inputText

                    let stopWordsFile = lookup "stopWords" files
                    stopWords <- case stopWordsFile of
                        Just swFile -> do
                            let swContent = BL.toStrict $ fileContent swFile
                            return $ map (map toLower) $ words $ T.unpack $ decodeUtf8 $ BL.fromStrict swContent
                        Nothing -> return defaultStopWords

                    Scotty.liftIO $ putStrLn $ "Stop words in use: " ++ unwords stopWords

                    result <- Scotty.liftIO $ processKWIC stopWords phrases
                    Scotty.liftIO $ putStrLn $ "Result sent:\n" ++ unlines result

                    Scotty.json $ ResultData result

                Nothing -> do
                    Scotty.status badRequest400
                    Scotty.text "Error: No file found in the upload"