{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import KWIC
import Web.Scotty hiding (put, body)
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Data.Text.Lazy as T
import Data.Char (toLower)
import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON) -- Importa FromJSON e ToJSON
import GHC.Generics (Generic) -- Importa Generic para derivar automaticamente
import Data.Aeson.Types()

-- Definição de uma instância de FromJSON para o tipo de dado esperado
newtype InputData = InputData { input :: T.Text } deriving (Show, Generic)

instance FromJSON InputData
instance ToJSON InputData

main :: IO ()
main = do
    stopWordsContent <- readFile "data/stop_words.txt"
    let stopWords = map (map toLower) $ words stopWordsContent

    scotty 3000 $ do
        middleware logStdoutDev
        middleware $ staticPolicy (addBase "static")

        Scotty.get "/" $ file "static/index.html"

        post "/process" $ do
            body <- jsonData :: ActionM InputData
            let inputText = input body
            liftIO $ putStrLn $ "Input recebido: " ++ T.unpack inputText
            let phrases = lines $ T.unpack inputText
            result <- liftIO $ processKWIC stopWords phrases
            liftIO $ putStrLn $ "Resultado enviado: " ++ show result
            json result

processKWIC :: [String] -> [String] -> IO [String]
processKWIC stopWords inputPhrases = 
    execStateT (put inputPhrases >> filterCharsInState) [] >>= \filteredPhrases ->
    evalStateT (toDict >>= \dict -> return (map formatResult (srtd (shift stopWords dict)))) filteredPhrases

formatResult :: (String, String) -> String
formatResult (shifted, source) = shifted ++ " (" ++ source ++ ")"
