module Solver where

import Data.List (nub)
-- import Debug.Trace (trace)

import Cell (Cell(..), intsToCells)
import Board (Board(..), printBoard)
import Order (cellsOrderLeftRight)

ints = [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]
cells = cellsOrderLeftRight [] $ intsToCells ints
cell = cells !! 0
cell' = cell {getValue = 1 + getValue cell}

board = Board {
  getCells = cells,
  getPointer = 0
}

-- solve :: Board -> Board 
-- solve cs
--   | isNo0OnBoard cs = cs
--   | isOver9Board cs = solve $ nextBoard $ trackBackBoard cs
--   | otherwise = solve $ nextBoard $ moveCursorToNext cs

boardNext :: Board -> Board
boardNext b@Board{ getPointer = p, getCells = cs }
  | True == validateCellAtPointer b' = b'
  | otherwise = boardNext b'
  where c = cs !! p
        c' = c { getValue = 1 + getValue c } 
        b' = replaceCellAtPointer c' b
        

replaceCellAtPointer :: Cell -> Board -> Board
replaceCellAtPointer c b = Board{ getCells = cs, getPointer = p }
  where p = getPointer b
        (b1,_:b2) = splitAt p $ getCells b 
        cs = b1 ++ [c] ++ b2 

validateCellAtPointer :: Board -> Bool
validateCellAtPointer b = all (validateCondition b) [boardToRow, boardToColumn, boardToKvadrant]

validateCondition :: Board -> (Board -> [Int]) -> Bool
validateCondition b checkFunction = ints == nub ints
  where ints = filter (0 /=) $ checkFunction b  

boardToRow :: Board -> [Int]
boardToRow Board{getPointer=p, getCells=cs} = [getValue c | c <-cs, getColumn c == column]
  where column = getColumn $ cs !! p

boardToColumn :: Board -> [Int]
boardToColumn Board{getPointer=p, getCells=cs} = [getValue c | c <-cs, getRow c == column]
  where column = getRow $ cs !! p

boardToKvadrant :: Board -> [Int]
boardToKvadrant Board{getPointer=p, getCells=cs} = [getValue c | c <-cs, getKvadrant c == kvadrant]
  where kvadrant = getKvadrant $ cs !! p

