import CellSeq (intsToCells)
import SolverSeq (solve, solveDo)
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
-- Rewrite slow replaceCell function using Set 
-- Make Solver import two functions solve and solveAsSteps
-- solve just gives result
-- solveAsSteps gives a list of lists of ints, for each calculation step
-- Add some checks to validate sudoku board before solving
-- Use maybe because it maybe impossible to solve a board
