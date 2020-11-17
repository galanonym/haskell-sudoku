module Printer (printBoard) where

import Cell (Cell(..))

-- export

printBoard :: [Cell] -> IO ()
printBoard = putStr . cellsToString

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
intsToChars xs = map (head . show) xs -- show returns strings like "1" therefore head to convert to '1'

cellsToInts :: [Cell] -> [Int]
cellsToInts = map getValue
