import Parser (doc)
import DocumentGenerator (generate)

parse :: String -> String
parse = (generate . doc)
