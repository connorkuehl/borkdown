module Parse where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char hiding (ord)

type Document  = [Paragraph]

type Paragraph = Block

data Block = Heading Int String
           | Para [Inline]
           | OrdList [Item]
           | UnordList [Item]
           | Blank
  deriving (Eq, Show)

data Item = Ordered [Inline]
          | Unordered [Inline]
  deriving (Eq, Show)

data Inline = Plain String
            | Link String String
            | Italic Inline
            | Bold Inline
            | Strike Inline
            | Code String
  deriving (Eq, Show)

-- parseDoc parses a string that represents an entire Markdown
-- document into its block representation for document generation.
parseDoc :: String -> Document
parseDoc = coalesce . removeBlanks . map toBlock . lines
  where
    -- Remove all other unnecessary blank lines.
    removeBlanks            :: [Block] -> [Block]
    removeBlanks []         = []
    removeBlanks (Blank:bs) = removeBlanks bs
    removeBlanks (b:bs)     = b:removeBlanks bs

    -- Combine all adjacent list item blocks
    coalesce :: [Block] -> [Block]
    coalesce [] = []
    coalesce (OrdList fs:OrdList ss:is) = coalesce (OrdList (fs ++ ss):is)
    coalesce (UnordList fs:UnordList ss:is) = coalesce (UnordList (fs ++ ss):is)
    coalesce (i:is) = i:coalesce is

-- This classification function was written by Mark Jones
-- under the name "classify". I've extended it to recursively
-- descend and produce inline elements.
toBlock :: String -> Block
toBlock line | all isSpace line = Blank
             | isOrd line = case (parse ord "" line) of
                              Right s -> OrdList ([Ordered (getItem line s)])
             | isUnord line = case (parse unOrd "" line) of
                              Right s -> UnordList ([Unordered (getItem line s)])
             | otherwise = case span ('#'==) line of
                             ([], s) -> Para ((compress . inlines) s)
                             (hs, s) -> Heading (length hs) (dropWhile isSpace s)
  where
    -- compress will concatenate all adjacent Plain text inline elements.
    -- There are multiple of them as a consequence of how the document is
    -- parsed.
    compress                      :: [Inline] -> [Inline]
    compress []                   = []
    compress (Plain s:Plain p:is) = compress (Plain (s ++ p):is)
    compress (i:is)               = i:compress is
    isOrd l = case (parse ord "" l) of
                Left _ -> False
                Right _ -> True
    isUnord l = case (parse unOrd "" l) of
                  Left _ -> False
                  Right _ -> True
    getItem line lead = (compress . inlines) (drop (length lead) line)

-- inlines recursively parses the contents of a paragraph block and
-- produces any inline elements that it fines (links, bold, italics,
-- code, etc)
inlines      :: String -> [Inline]
inlines []   = []
inlines line = case (parse (choice inlineElements) "" line) of
                    -- Left means we failed to parse an inline element so
                    -- it will be rendered as plaintext
                    Left _ -> Plain [(head line)]:inlines (tail line)
                    -- Otherwise we succeeded in finding an inline element
                    -- and we'll continue to look for others. Dropping the
                    -- parsed characters from the string to prevent duplication.
                    Right i -> i:inlines (drop (inlineLength i) line)

-- inlineLength returns the length of the inline element
-- This is used primarily for dropping consumed characters
-- from the input string.
--
-- You'll see here that I'm adding the length of other
-- strings to these and that is to account for the fact
-- that Parsec consumes the symbols that denote an inline
-- element so I am adding them back here.
inlineLength            :: Inline -> Int
inlineLength (Plain s)  = length s
inlineLength (Bold b)   = inlineLength b + (length "****")
inlineLength (Italic i) = inlineLength i + (length "**")
inlineLength (Link d u) = (length (d ++ u)) + (length "()[]")
inlineLength (Code c)   = (length c) + (length "``")
inlineLength (Strike s) = (inlineLength s) + (length "~~~~")

-- inlineElements is a collection of parsers that will attempt
-- to parse inline elements.
inlineElements :: [Parser Inline]
inlineElements = [(try bold <|> try italic <|> strike), link, code]

-- Parsers!

text :: Parser Inline
text = try bold <|> try italic <|> try strike <|> try link <|> plain

plain :: Parser Inline
plain = do c <- anyChar -- Explicitly take one char to avoid empty strings
           rest <- manyTill anyChar (lookAhead (oneOf "*~"))
           return $ Plain (c:rest)

bold :: Parser Inline
bold = Bold <$> between (string "**") (string "**") text

italic :: Parser Inline
italic = Italic <$> between (string "*") (string "*") text

strike :: Parser Inline
strike = Strike <$> between (string "~~") (string "~~") text

link :: Parser Inline
link = do desc <- between (char '[') (char ']') (manyTill anyChar (lookAhead (char ']')))
          url <- between (char '(') (char ')') (manyTill anyChar (lookAhead (char ')')))
          return $ Link desc url

code :: Parser Inline
code = Code <$> between (char '`') (char '`') (manyTill anyChar (lookAhead (char '`')))

ord :: Parser String
ord = do num <- many1 digit
         dot <- char '.'
         space <- char ' '
         return $ num ++ [dot] ++ [space]

unOrd :: Parser String
unOrd = do star <- char '*'
           space <- char ' '
           return $ [star] ++ [space]
