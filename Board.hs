module Board (Board(..), printBoard) where

import Cell (Cell(..))

-- export

data Board = Board {
  getCells :: [Cell],
  getPointer :: Int
} deriving (Show)

printBoard :: Board -> IO ()
printBoard = putStr . cellsToString . getCells

-- private

cellsToString :: [Cell] -> String
cellsToString = flip (++) "\n" . insertBetweenString 12 '\n' . insertBetweenString 3 ' ' . intsToChars . cellsToInts

insertBetweenString :: Int -> Char -> String -> String 
insertBetweenString 0 _ xs = xs
insertBetweenString _ _ [] = []
insertBetweenString n char xs
 | length xs <= n = xs
 | otherwise = take n xs ++ [char] ++ insertBetweenString n char (drop n xs)

intsToChars :: [Int] -> [Char]
intsToChars xs = map (\x -> if (x >= 10) then '!' else  head $ show x) xs -- show returns strings like "1" therefore head to convert to '1'

cellsToInts :: [Cell] -> [Int]
cellsToInts = map getValue
