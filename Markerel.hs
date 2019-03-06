import Parser (parseMarkdown)
import DocumentGenerator

parse :: String -> String
parse = (generate . parseMarkdown)
