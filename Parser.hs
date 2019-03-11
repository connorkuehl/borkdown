module Parser where

import Data.Char

type Document = [Paragraph]

data Paragraph = Heading Int String
               | Prose [String]
  deriving Show

data Line = Head Int String
          | Blank
          | Text String
  deriving Show

doc :: String -> Document
doc = markdown . map classify . lines

markdown               :: [Line] -> Document
markdown []            = []
markdown (Head n h:ls) = Heading n h : markdown ls
markdown (Blank:ls)    = markdown ls
markdown (Text l:ls)   = text [l] ls

classify :: String -> Line
classify line | all isSpace line = Blank
              | otherwise = case span ('#'==) line of
                              ([], s) -> Text s
                              (hs, s) -> Head (length hs) (dropWhile isSpace s)

text                :: [String] -> [Line] -> Document
text ss (Text s:ls) = text (s:ss) ls
text ss ls          = Prose (reverse ss) : markdown ls
