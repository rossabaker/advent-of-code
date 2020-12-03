module Day03 where

newtype Grid = Grid {unGrid :: [[Char]]}

answer1 :: IO Int
answer1 = flip bonks (3, 1) <$> readGrid

answer2 :: IO Int
answer2 =
  product . flip (fmap . bonks) slopes <$> readGrid
  where
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

width :: Grid -> Int
width = length . head . unGrid

height :: Grid -> Int
height = length . unGrid

isTree :: Grid -> (Int, Int) -> Bool
isTree grid (x, y) =
  unGrid grid !! y !! x' == '#'
  where
    x' = mod x (width grid)

path :: Int -> Int -> [(Int, Int)]
path x y = zip [0, x ..] [0, y ..]

bonks :: Grid -> (Int, Int) -> Int
bonks grid (x, y) =
  length $ filter (isTree grid) path'
  where
    path' = takeWhile ((< height grid) . snd) $ path x y

example :: Grid
example =
  Grid
    [ "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
    ]

readGrid :: IO Grid
readGrid = Grid . lines <$> readFile "input03.txt"
