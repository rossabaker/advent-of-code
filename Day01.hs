module Day01
  ( part1,
  )
where

import Data.List (tails)
import Data.Maybe (listToMaybe)

part1 :: IO (Maybe Int)
part1 = answer 2 <$> readExpenses

part2 :: IO (Maybe Int)
part2 = answer 3 <$> readExpenses

answer :: Int -> [Int] -> Maybe Int
answer n xs =
  listToMaybe $
    product
      <$> filter ((== 2020) . sum) (combinations n xs)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = do
  x : xs' <- tails xs
  ys <- combinations (n - 1) xs'
  pure (x : ys)

exampleInputs :: [Int]
exampleInputs = [1721, 979, 366, 299, 675, 1456]

readExpenses :: IO [Int]
readExpenses = fmap read . lines <$> readFile "input01.txt"
