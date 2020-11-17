module Solver where

import Data.List (nub)
import Debug.Trace (trace)

import Cell (Cell(..), intsToCells)
import Printer (printBoard)

ints = [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]

cells = intsToCells ints

cell = cells !! 0

type Pointer = Int

cellsToRow :: Pointer -> [Cell] -> [Int]
cellsToRow p cs = [getValue c | c <-cs, getColumn c == column]
  where column = getColumn $ cs !! p

cellsToColumn :: Pointer -> [Cell] -> [Int]
cellsToColumn p cs = [getValue c | c <-cs, getRow c == column]
  where column = getRow $ cs !! p

cellsToKvadrant :: Pointer -> [Cell] -> [Int]
cellsToKvadrant p cs = [getValue c | c <-cs, getKvadrant c == kvadrant]
  where kvadrant = getKvadrant $ cs !! p

-- either:
  -- cursor jumps over isFixed cells
  -- isFixed cells are ordered to the back and errors on incrementing value

-- order cells
-- cellsOrderLeftRight :: [Cell] -> [Cell]
cellsOrderLeftRight acc [] = acc
cellsOrderLeftRight acc (c:cs) = cellsOrderLeftRight acc' cs
  where acc' = if (getValue c == 0) then c:acc else acc ++ [c]
