import DocGen
import Parse

markerel :: String -> String
markerel = generateHtml . parseDoc
