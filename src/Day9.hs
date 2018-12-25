module Main where

import System.Environment
import Data.Map (Map(..), foldr, empty, adjust, insert, member)
import Data.Maybe (fromJust)
import Data.List
import Control.DeepSeq
--import Debug.Trace

type Player = Integer
type Marble = Integer
type Score  = Integer

type Circle     = [Marble]
type Scoreboard = Map Player Score

data GameState = GameState {
        numPlayers :: Player
      , maxMarble  :: Marble
      , circle     :: Circle
      , currMarble :: Marble
      , currPlayer :: Player
      , scoreboard :: Scoreboard
    }

-- input looks like this:
-- 439 players; last marble is worth 71307 points
parseInput :: String -> (Player, Marble)
parseInput s =
    (nthWord 0, nthWord 6)
    where
        nthWord i = read (words s !! i)

-- start playing with a 0 marble in the circle, current marble is 0,
-- current player is 0, scoreboard is empty
startState :: Player -> Marble -> GameState
startState p m =
    GameState {
        numPlayers = p
      , maxMarble  = m
      , circle     = [ 0 ]
      , currMarble = 0
      , currPlayer = 0
      , scoreboard = empty
    }

maxScore :: Player -> Marble -> Score
maxScore p m =
    Data.Map.foldr max 0 (scoreboard finalGameState)
    where
        finalGameState = foldl' (\gs nextMarble ->
            let
                output = force (placeMarble nextMarble gs)
            in
                output) (startState p m) [1..m]

placeMarble :: Marble -> GameState -> GameState
placeMarble i gs
    | (i `mod` 23) == 0  =
        gs {
            circle     = updateCircle23 (circle gs) (currMarble gs)
          , currMarble = updateCurrMarble23 (circle gs) (currMarble gs)
          , currPlayer = nextPlayer
          , scoreboard = updateScoreboard (scoreboard gs) (currPlayer gs) (circle gs) i (currMarble gs)
        }
    -- "normal" case, update circle
    | otherwise          =
        gs {
            circle = updateCircle (circle gs) (currMarble gs) i
          , currMarble = i
          , currPlayer = nextPlayer
        }
    where
        nextPlayer = (currPlayer gs + 1) `mod` (numPlayers gs)

-- normal update case, add new marble in the correct position (2 to the right
-- of the current marble)
updateCircle :: Circle -> Marble -> Marble -> Circle
updateCircle circle currMarble i =
    take n circle ++ [i] ++ drop n circle
    where
        n = (fromJust (elemIndex currMarble circle) + 1) `mod` (length circle) + 1

-- remove marble 7 to the "left" of the currentMarble
updateCircle23 :: Circle -> Marble -> Circle
updateCircle23 circle currMarble =
    filter (/=marbleToBeRemoved) circle
    where
        marbleToBeRemoved = marbleNCounterClockWise circle currMarble 7 

-- current Marble is now the marble 6 to the "left" of currentMarble
updateCurrMarble23 :: Circle -> Marble -> Marble
updateCurrMarble23 circle currMarble =
    marbleNCounterClockWise circle currMarble 6

-- find the marble n counterclockwise from current marble
marbleNCounterClockWise :: Circle -> Marble -> Int -> Marble
marbleNCounterClockWise circle currMarble n =
    circle !! ((currIndex - n) `mod` length circle)
    where
        currIndex = fromJust $ elemIndex currMarble circle

-- updated the scoreboard based on the marble to be placed and the marble 7
-- to the left
updateScoreboard :: Scoreboard -> Player -> Circle -> Marble -> Marble -> Scoreboard
updateScoreboard scoreboard currPlayer circle i currMarble =
    if currPlayer `member` scoreboard then
        adjust (+ points) currPlayer scoreboard
    else
        Data.Map.insert currPlayer points scoreboard
    where
        points = i + marbleNCounterClockWise circle currMarble 7

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    let
        (players, lastMarble) = parseInput content
    print $ maxScore players lastMarble
