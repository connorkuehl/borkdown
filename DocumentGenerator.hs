module DocumentGenerator (generate) where

import Parser hiding (text)

generate :: Document -> String
generate = document

document    :: Document -> String
document ps = "<html><body>" ++ concat (map paragraph ps) ++ "</body></html>"

paragraph :: Paragraph -> String
paragraph (Heading i s) = "<h1>" ++ s ++ "</h1>"
paragraph (Prose ts) = "<p>" ++ concat (map text ts) ++ "</p>"

text :: Text -> String
text (Plain s) = s
text (Bold b) = "<b>" ++ text b ++ "</b>"
text (Italic i) = "<i>" ++ text i ++ "</i>"
