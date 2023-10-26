module A1 where

import Data.Char (toUpper)

-- *** Assignment 1-1 *** --

-- Q#01

_SIZE_ :: Int
_SIZE_ = 3

-- Q#02

_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03

convertRowIndex :: Char -> Int
convertRowIndex c = fromEnum (toUpper c) - 65

-- Q#04

_INVALID_MOVE_ :: (Int, Int) 
_INVALID_MOVE_ = (-1, -1) 

-- Q#05

_SEP_ = "_|_"

-- *** Assignment 1-2 *** --

-- Q#06
data Square = X | O | Void deriving (Show, Eq)

-- Q#07
data GameState = Xwon | Owon | Tie | Progress deriving (Show, Eq)



-- Q#08

type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)


-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer input =
  if input == True
    then X
  else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ input
  | input == True = X
  | input == False = O

-- Q#10

showGameState :: GameState -> String
showGameState gs = case gs of
  Xwon -> "X won the game"
  Owon -> "O won the game"
  Tie -> "The game is a tie"
  Progress -> "The game is in progress"

-- Q#11

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer Void = Void


-- Q#12

showSquare :: Square -> String
showSquare square
  | square == X = "X"
  | square == O = "O"
  | square == Void = "_"