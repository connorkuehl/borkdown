module Parser(parseMarkdown) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char

type Document = [Paragraph]

data Paragraph = Heading Int String
               | Prose [Text]
  deriving Show

data Text = Plain String
          | Bold String
          | Italic String
  deriving Show

parseMarkdown   :: String -> Document
parseMarkdown s = case (parse doc "" s) of
                    Left err -> []
                    Right d -> d

doc :: Parser Document
doc = para `sepBy` eop

eop :: Parser String
eop = count 2 endOfLine

para :: Parser Paragraph
para = try heading <|> prose

heading :: Parser Paragraph
heading = do hs <- many1 (char '#')
             skipMany space
             h <- many alphaNum
             return $ Heading (length hs) h

prose :: Parser Paragraph
prose = Prose <$> (:[]) <$> text

text :: Parser Text
text = try bold <|> try italic <|> plain

plain :: Parser Text
plain = Plain <$> manyTill anyChar (try (lookAhead (oneOf "*\n")))

bold :: Parser Text
bold = Bold <$> between (string "**") (string "**") (manyTill anyChar (try (lookAhead (string "**"))))

italic :: Parser Text
italic = Italic <$> between (string "*") (string "*") (manyTill anyChar (try (lookAhead (char '*'))))
