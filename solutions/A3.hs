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
formatRows rows = map (formatLine . showSquares) rows

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
dropFirstCol rows = map (\(x:xs) -> xs) rows


dropLastCol :: Board -> Board
dropLastCol rows = map (\row -> init row) rows

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

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined