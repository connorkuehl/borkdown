module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char

type Document = [Paragraph]

data Paragraph = Heading Int String
               | Prose [Text]
  deriving Show

data Text = Plain String
          | Bold Text
          | Italic Text
  deriving Show

parseMarkdown   :: String -> Document
parseMarkdown s = case (parse doc "" s) of
                    Left err -> []
                    Right d -> d

doc :: Parser Document
doc =  para `sepEndBy` eop

eop = (count 2 endOfLine)

para :: Parser Paragraph
para = try heading <|> prose

heading :: Parser Paragraph
heading = do hs <- many1 (char '#')
             spaces
             h <- manyTill anyChar ((lookAhead eop) <|> (eof >> return []))
             return $ Heading (length hs) h

prose :: Parser Paragraph
prose = Prose <$> many1 text

text :: Parser Text
text = formatted

formatted :: Parser Text
formatted = try bold <|> try italic <|> plain

plain :: Parser Text
plain = Plain <$> manyTill anyChar (try (lookAhead (string "*" <|> eop)))

bold :: Parser Text
bold = Bold <$> between (string "**") (string "**") formatted

italic :: Parser Text
italic = Italic <$> between (string "*") (string "*") formatted
