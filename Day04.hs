module Day04 where

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

answer1 :: IO Int
answer1 = length . filter valid <$> readPassports

requiredFields :: Set String
requiredFields = Set.fromList [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

valid :: Set String -> Bool
valid passport = Set.isSubsetOf requiredFields passport

passports :: [String] -> [Set String]
passports lines = go lines Set.empty []
  where
    go :: [String] -> (Set String) -> [Set String] -> [Set String]
    go [] fields acc = fields:acc
    go ("":lines') fields acc = go lines' Set.empty (fields:acc)
    go (line:lines') fields acc = go lines' (fields <> parseFields line) acc

parseFields :: String -> Set String
parseFields line = Set.fromList $ parseField <$> words line

parseField :: String -> String
parseField line = head $ splitOn ":" line

example :: [String]
example = [
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
  "byr:1937 iyr:2017 cid:147 hgt:183cm",
  "",
  "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
  "hcl:#cfa07d byr:1929",
  "",
  "hcl:#ae17e1 iyr:2013",
  "eyr:2024",
  "ecl:brn pid:760753108 byr:1931",
  "hgt:179cm",
  "",
  "hcl:#cfa07d eyr:2025 pid:166559648",
  "iyr:2011 ecl:brn hgt:59in"
  ]

readPassports :: IO [Set String]
readPassports = passports . lines <$> readFile "input04.txt"
