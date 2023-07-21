import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  inputJsonFile <- BS.readFile "input.json"
  inputTemplateFile <- TIO.readFile "template.txt"
  print inputTemplateFile

  case decode inputJsonFile :: Maybe Object of
    Nothing -> putStrLn "Error parsing JSON"
    Just decodedJson -> print decodedJson

replaceWordInString :: T.Text -> T.Text -> T.Text -> T.Text
replaceWordInString oldWord newWord = T.intercalate newWord . T.splitOn oldWord