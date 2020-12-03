module Day02 where

import Data.Either (rights)
import Text.Parsec
import Text.Parsec.String (Parser)

data Entry = Entry
  { entryMin :: Int,
    entryMax :: Int,
    entryC :: Char,
    entryPassword :: String
  }
  deriving (Show)

answer1 :: IO Int
answer1 = length . filter validate1 <$> readEntries

answer2 :: IO Int
answer2 = length . filter validate2 <$> readEntries

validate1 :: Entry -> Bool
validate1 e =
  (count >= entryMin e) && (count <= entryMax e)
  where
    count = length $ filter (== entryC e) (entryPassword e)

validate2 :: Entry -> Bool
validate2 e =
  (entryC e == x) /= (entryC e == y)
  where
    x = entryPassword e !! (entryMin e - 1)
    y = entryPassword e !! (entryMax e - 1)

entryParser :: Parser Entry
entryParser = do
  min <- read <$> many1 digit
  char '-'
  max <- read <$> many1 digit
  space
  c <- anyChar
  char ':'
  space
  password <- manyTill anyToken eof
  pure $ Entry min max c password

examples :: [String]
examples =
  [ "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc"
  ]

readEntries :: IO [Entry]
readEntries = rights . fmap (parse entryParser "") . lines <$> readFile "input02.txt"
