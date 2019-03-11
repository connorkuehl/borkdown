module DocumentGenerator (generate) where

import Parser hiding (text)

generate :: Document -> String
generate = document

document    :: Document -> String
document ps = "<html><body>" ++ concat (map paragraph ps) ++ "</body></html>"

paragraph :: Paragraph -> String
paragraph (Heading i s) = "<h" ++ show i ++ ">" ++ s ++ "</h" ++ show i ++ ">"
paragraph (Prose ts) = "<p>" ++ concat ts ++ "</p>"
