module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01

_HEADER_ = [' '] ++ formatLine (map show _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares squares = map showSquare squares

-- Q#03

dropFirstCol :: Board -> Board
dropFirstCol rows = map (\(x:xs) -> xs) rows

-- Q#04

dropLastCol :: Board -> Board
dropLastCol rows = map (\row -> init row) rows

--Q#05

formatRows :: [Row] -> [String]
formatRows rows = map (formatLine . showSquares) rows


-- Q#06

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ player line =
  length results == 3 where
  results = filter (\square -> square == player) line

-- *** Assignment 4-2 *** --

-- Q#07

isWinningLine :: Player -> Line -> Bool
isWinningLine _       []    = False
isWinningLine player  line  =
  foldr (\square acc -> if acc == False then False else square == player) True line

-- Q#08

hasWon :: Player -> Board -> Bool
hasWon player []    = False
hasWon player board =
  foldr (\line acc -> if acc == True then True else isWinningLine player line) False (getAllLines board)

-- Q#09

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

getGameState :: Board -> GameState
getGameState board
  | hasWon X board == True    = Xwon
  | hasWon O board == True    = Owon
  | isTied board == True      = Tie
  | otherwise                 = Progress


playMove :: Player -> Board -> Move -> (GameState, Board)
playMove player board move = (getGameState newBoard, newBoard) where
  newBoard = putSquare player board move

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices rows = zipWith (\c s -> [c] ++ s) ['A'..] rows

-- Q#11

formatBoard :: Board -> String
formatBoard rows =
  unlines $ [_HEADER_] ++ (prependRowIndices . formatRows $ rows)