module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)
import System.IO
import Data.Bool (bool)

-- *** Assignment 5-1 *** --

-- Q#01

printBoard :: Board -> IO ()
printBoard board = putStrLn $ formatBoard board

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= \s -> putStrLn s

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= \t -> return (getFirstPlayer t)

-- Q#04

getMove :: Board -> IO Move
getMove board =
  getLine >>= \moveStr ->
  go moveStr where
    go :: String -> IO Move
    go x =
      let move = stringToMove x in
      if isValidMove board move
      then
        return move
      else
        putStrLn "Invalid move! Try again" >>
        getMove board

-- Q#05

play :: Board -> Player -> IO ()
play board player =
  when _DISPLAY_LOGO_ printLogo >>
  printBoard board >>
  putStrLn (promptPlayer player) >>
  getMove board >>= \move ->
    let (state, newBoard) = playMove player board move in
      if state == Progress
      then play newBoard (switchPlayer player)
      else
        printBoard newBoard >>
        putStrLn (showGameState state)

-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo = undefined

-- Q#08

firstPlayerDo = undefined

-- Q#09

getMoveDo = undefined

-- Q#10

playDo = undefined