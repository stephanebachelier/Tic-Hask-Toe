module A3 where

import A1
import A2

import Data.List (transpose)

-- *** Assignment 3-1 ***

-- Q#01

showInts :: [Int] -> [String]
showInts []       = []
showInts [x]      = [show x]
showInts (x:xs)   = showInts [x] ++ showInts xs


_HEADER_ = [' '] ++ (formatLine $ showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares []      = []
showSquares [x]     = [showSquare x]
showSquares (x:xs)  = [showSquare x] ++ showSquares xs


-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (r:rs) = [(formatLine . showSquares $ r)] ++ formatRows rs

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty []       _   = False
isColEmpty (x:xs)   0   = x == Void
isColEmpty (x:y:z)  n
  | n < 0         = isColEmpty [] n
  | n >= _SIZE_   = isColEmpty [] n
  | n == 1        = isColEmpty ([x] ++ [y] ++ z) 0
  | n == 2        = isColEmpty (z ++ [x] ++ [y]) 0

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol ((x:xs):tail) = [xs] ++ dropFirstCol tail


dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (x:xs) = [init x] ++ dropLastCol xs

-- Q#06
getDiag1 :: Board -> Line
getDiag1 [row]  = [head row]
getDiag1 (x:xs) = getDiag1 [x] ++ getDiag1 (dropFirstCol xs)


getDiag2 :: Board -> Line
getDiag2 [row]  = [last row]
getDiag2 (x:xs) = getDiag2 [x] ++ getDiag2 (dropLastCol xs)

getAllLines :: Board -> [Line]
getAllLines rows = rows ++ transpose rows ++ [getDiag1 rows, getDiag2 rows]

-- *** Assignment 3-2 ***

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p (r:rs) (0,y) = (replaceSquareInRow p y r):rs
putSquare p (r:rs) (x,y) = [r] ++ putSquare p rs (x-1,y)

-- Q#08
prependRowIndices :: [String] -> [String]
prependRowIndices rows = go (indexRowStrings rows) where
  go :: [(Char, String)] -> [String]
  go [] = []
  go ((c,s):xs) = ([c] ++ s): go xs

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine _       []    = False
isWinningLine player  line  = go False line where
  go :: Bool -> [Square] -> Bool
  go acc []       = acc
  go acc (sq:sqs) = if sq /= player then False else go True sqs

-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove board move = 
  if isMoveInBounds move == False 
  then False 
  else go board move where
    go :: Board -> Move -> Bool
    go []     _       = False 
    go (r:rs) (0, y)  = isColEmpty r y
    go (r:rs) (x, y)  = go rs (x - 1, y)
