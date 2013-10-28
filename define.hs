import qualified Text.JSON as JS
import Network.HTTP
import Data.Text

main :: IO()
main = do
    response <- simpleHTTP $ getRequest "http://ip.jsontest.com/"
    response_string <- getResponseBody response
    case JS.decode response_string of
        JS.Ok val -> print (val :: JS.JSValue)
        JS.Error err -> error err
    putStrLn response_string
