{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (toList)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

main :: IO ()
main = do
  commandLineArgs <- getArgs

  inputTemplateFile <- TIO.readFile $ head commandLineArgs
  inputJsonFile <- BS.readFile $ commandLineArgs !! 1

  case decode inputJsonFile :: Maybe Object of
    Nothing -> putStrLn "Error parsing JSON"
    Just decodedJson -> TIO.writeFile "out.txt" $ populateTemplate inputTemplateFile decodedJson

populateTemplate :: T.Text -> Object -> T.Text
populateTemplate inputText keyValuePairs = populateTemplateHelper inputText $ toList keyValuePairs
  where
    populateTemplateHelper :: T.Text -> [(Key, Value)] -> T.Text
    populateTemplateHelper inputText [] = inputText
    populateTemplateHelper inputText ((key, value) : xs) = populateTemplateHelper (replaceWordInString (prepareKey key) (valueToText value) inputText) xs

replaceWordInString :: T.Text -> T.Text -> T.Text -> T.Text
replaceWordInString oldWord newWord = T.intercalate newWord . T.splitOn oldWord

prepareKey :: Key -> T.Text
prepareKey key = "{{" <> toText key <> "}}"

valueToText :: Value -> T.Text
valueToText (String foo) = foo
valueToText (Number foo) = T.pack $ show foo
valueToText (Bool foo) = boolToText foo
valueToText _ = T.empty

boolToText :: Bool -> T.Text
boolToText True = "True"
boolToText False = "False"