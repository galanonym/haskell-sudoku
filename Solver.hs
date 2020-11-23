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

-- @todo 
  -- add printBoardSorted function for showing board with original sort
  -- add Main/Sudoku.hs with IO
  -- profile for speed
  -- add better ordering function cellsOrderDenseFirst

solve :: Board -> Board 
solve b
  | boardIsNo0s b = b
  | boardIsOver9 b = solve $ boardIncrement $ boardTrackBack b 
  | otherwise = solve $ boardIncrement $ boardCursorToNext b 

boardIsNo0s :: Board -> Bool
boardIsNo0s Board{ getCells = cs } = [] == filter ((0==) . getValue) cs

boardIsOver9 :: Board -> Bool
boardIsOver9 Board{ getCells = cs } = not $ null $ filter ((9<) . getValue) cs

boardCursorToNext :: Board -> Board
boardCursorToNext Board { getPointer = p, getCells = cs } = Board { getPointer = p + 1, getCells = cs }

boardTrackBack :: Board -> Board
boardTrackBack b@Board{ getPointer = p, getCells = cs }
  | pTB < 0 = error "Trackback not possible"
  | otherwise = bTB
    where c = cs !! p
          c0 = c { getValue = 0 }
          b0 = replaceCellAtPointer c0 b
          csTB = getCells b0 -- TB - track back
          pTB = p - 1
          bTB = Board { getPointer = pTB, getCells = csTB }

boardIncrement :: Board -> Board
boardIncrement b@Board{ getPointer = p, getCells = cs }
  | True == validateCellAtPointer b' = b'
  | otherwise = boardIncrement b'
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
