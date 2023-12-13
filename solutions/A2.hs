{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)

-- *** Assignment 2-1 *** --

-- Q#01

promptPlayer :: Player -> String
-- promptPlayer p = "Player " ++ show p ++ "'s turn: enter a row and column position (ex. A1)"
promptPlayer p = concat ["Player ", show p, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02

_RANGE_ :: [Int]
_RANGE_ = [0 .. _SIZE_ - 1]

-- Q#03

isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit c
  | isDigit c == True = read [c]
  | isDigit c == False = -1

-- Q#04

_EMPTY_ROW_ = replicate _SIZE_ Void


_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied board =
  notElem Void (concat board)

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings stringList =
  zip ['A'..] stringList

-- Q#07

formatLine :: [String] -> String
formatLine stringList =
  concat [_SEP_, (intercalate _SEP_ stringList), _SEP_]

-- *** Assignment 2-2 *** --

-- Q#08

isMoveInBounds = undefined

-- Q#09

stringToMove = undefined

-- Q#10

replaceSquareInRow = undefined