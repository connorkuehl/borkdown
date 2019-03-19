import DocGen
import Parse
import System.IO

markerel :: String -> String
markerel = generateHtml . parseDoc

main :: IO ()
main = getContents >>= return . markerel >>= putStrLn
