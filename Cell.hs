module Cell (Cell(..), intsToCells) where

data Cell = Cell {
    unCursor :: Bool,
    unOrder :: Int,
    unPositionX :: Int,
    unPositionY :: Int,
    unValue :: Int
  } deriving (Show, Eq, Ord)
-- Cell { unCursor = False, unOrder = Nothing, unValue = 5, unPositionX = 0, unPositionY = 0 }

-- testing
ints :: [Int]
ints = [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]

cells :: [Cell]
cells = intsToCells ints

checkLength :: [Int] -> [Int]
checkLength list 
  | 81 == (length list) = list
  | otherwise = error "Input not valid sudoku board"

type PositionX = Int
type PositionY = Int

convertToTriples :: [Int] -> [(PositionX, PositionY, Int)]
convertToTriples vs = map (\i -> (xs !! i, ys !! i, vs !! i)) [0..80] 
  where xs = concat $ replicate 9 [0..8]
        -- [1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9,1,2...]
        ys = concat $ map (replicate 9) [0..8]
        -- [1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3...]
-- [(1,1,5),(2,1,3),(3,1,0),(4,1,0),(5,1,7),(6,1,0),(7,1,0)...]

convertToCells :: [(PositionX, PositionY, Int)] -> [Cell]
convertToCells ts = map (\triple -> Cell {
    unCursor = False,
    unOrder = (-1), -- -1 means that it needs to be filled 
    unPositionX = first triple,
    unPositionY = second triple, 
    unValue = third triple
  }) ts 
  where 
    first (x, _, _) = x
    second (_, x, _) = x
    third (_, _, x) = x
-- [Cell {unCursor = False, unOrder = Nothing, unValue = 5, unPositionX = 1, unPositionY = 1},Cell {unCursor = False, unOrder = Nothing, unValue = 3, unPositionX = 2, unPositionY = 1},Cell {unCursor = False, unOrder = Just 0, unValue = 0, unPositionX = 3, unPositionY = 1}...]

intsToCells :: [Int] -> [Cell]
intsToCells = fillOrder 0 . convertToCells . convertToTriples . checkLength

-- Unprefilled
fillOrder :: Int -> [Cell] -> [Cell]
fillOrder _ [] = []
fillOrder i (c:cs)
  | 0 /= unValue c = c : (fillOrder i cs) -- prefilled cells have 0 as value
  | otherwise = c' : (fillOrder (i + 1) cs) 
    where c' = c { unOrder = i }

findOrderHighest (c:cs) = foldr binF accStart cs
  where binF c acc = if unOrder c > acc then unOrder c else acc
        accStart = unOrder c

-- fillOrderPrefilled :: Int -> [Cell] -> [Cell]

-- setCursorOnFirst
