import Control.Monad
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy
import Data.String
import Data.Text
import qualified Data.HashMap.Strict as DHS
import Network.HTTP
import System.Environment
import Text.Pandoc


main :: IO()
main = do

    args <- getArgs

    let queryString = "http://en.wiktionary.org/w/api.php?format=json&action=query&prop=extracts&titles=" ++ Prelude.head args ++ "&redirects=true"
    response_string <- getResponseBodyFromURL queryString 

    let byteStringResponse = Data.ByteString.Lazy.pack $ Prelude.map c2w response_string;
    let x = (decode >=> anObject >=> DHS.lookup (Data.Text.pack "query") 
                    >=> anObject >=> DHS.lookup (Data.Text.pack "pages") 
                    >=> anObject >=> (\x -> Just (snd $ Prelude.head $ DHS.toList x)) -- Select first element of JSON array with unknown key (page id);
                    >=> anObject >=> DHS.lookup (Data.Text.pack "extract")) byteStringResponse 

    case x of 
      Just (String y) -> Prelude.putStrLn $ htmlToPlain $ Data.Text.unpack y 
      Nothing -> Prelude.putStrLn "err"
    Prelude.putStrLn "done"

htmlToPlain :: String -> String
htmlToPlain = 
  (writePlain def) . 
  readHtml def

getResponseBodyFromURL :: String -> IO String
getResponseBodyFromURL url = do
  response <- simpleHTTP $ getRequest url 
  getResponseBody response

-- Helper for passing values in Maybe context in the Kleisli composition (lol)
anObject :: Value -> Maybe Object
anObject (Object m) = Just m
anObject _          = Nothing

