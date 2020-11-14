module Cell (Cell(..), intsToCells) where

data Cell = Cell {
    unCursor :: Bool,
    unOrder :: Int,
    unPositionX :: Int,
    unPositionY :: Int,
    unValue :: Int
  } deriving (Show, Eq, Ord)
-- Cell { unCursor = False, unOrder = Nothing, unValue = 5, unPositionX = 0, unPositionY = 0 }

-- Testing
ints :: [Int]
ints = [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]

cells :: [Cell]
cells = intsToCells ints

-- Exported
intsToCells :: [Int] -> [Cell]
intsToCells ints = setCursorOnFirst $ fillOrderPrefilled ((findOrderHighest cellsVacant) + 1) cellsVacant
  where cellsVacant = fillOrderVacant 0 $ convertToCells $ convertToTriples $ checkLength ints

-- Other functions
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

fillOrderVacant :: Int -> [Cell] -> [Cell]
fillOrderVacant _ [] = []
fillOrderVacant i (c:cs)
  | 0 /= unValue c = c : (fillOrderVacant i cs) -- vacant cells have 0 as value
  | otherwise = c' : (fillOrderVacant (i + 1) cs) 
    where c' = c { unOrder = i }

findOrderHighest (c:cs) = foldr binF accStart cs
  where binF c acc = if unOrder c > acc then unOrder c else acc
        accStart = unOrder c

fillOrderPrefilled :: Int -> [Cell] -> [Cell]
fillOrderPrefilled _ [] = []
fillOrderPrefilled i (c:cs)
  | 0 /= unValue c = c' : (fillOrderPrefilled (i + 1) cs) 
  | otherwise = c : (fillOrderPrefilled i cs)
    where c' = c { unOrder = i }

setCursorOnFirst :: [Cell] -> [Cell]
setCursorOnFirst [] = []
setCursorOnFirst (c:cs)
  | 0 == unOrder c = c' : (setCursorOnFirst cs)
  | otherwise = c : (setCursorOnFirst cs)
    where c' = c { unCursor = True }
