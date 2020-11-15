import Cell (intsToCells)
import Solver (solve)
import Data.Char (digitToInt)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ints = inputToInts input
  let solved = solve $ intsToCells ints
  putStr $ show solved

-- input.txt:
-- 530 070 000
-- 600 195 000
-- 098 000 060

-- 800 060 003
-- 400 803 001
-- 700 020 006

-- 060 000 280
-- 000 419 005
-- 000 080 079

inputToInts :: [Char] -> [Int] 
inputToInts cs =  map digitToInt $ filter (\c -> elem c ['0'..'9']) cs

-- @todo
-- Move filling of unOrder values to next module
-- In Solver solve function, add "where c = findCellWithCursor cs" and inject it in trackBackBoard, moveCursorToNext, nextBoard, prevCell, nextCell
  -- This solution complcates a lot of code, much less clear, gives head empty list exceptions
-- Fix solver solve function to first solve then move cursor
-- Fix nextNotPrefilledCell to use unOrder
-- Fix previousNotPrefilledCell to use unOrder
-- unOrder does not need to be Maybe, we can order prefilled cells to be solved last
-- Cells should set cursor on first cell that is not prefilled
-- Solver should move to next cursor after the board is solved
-- Then nextNotPrefilledCell' would be much simpler, without case where there is no Cursor on board

-- Use unOrder to find next / previous cell
-- unOrder could be generated based on how populated are prefilled rows, columns and kvadrants 
-- Make Solver import two functions solve and solveAsSteps
-- solve just gives result
-- solveAsSteps gives a list of lists of ints, for each calculation step
-- Add some checks to validate sudoku board before solving
-- Use maybe because it maybe impossible to solve a board
