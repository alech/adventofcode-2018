{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Main where

import System.Environment
import Data.Map (Map(..), foldr, empty, adjust, insert, member)
import Data.Maybe (fromJust)
import Data.List
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.List.PointedList hiding (next, moveN, deleteRight)
import Data.List.PointedList.Circular (next, moveN, deleteRight)
import Debug.Trace

type Player = Integer
type Marble = Integer
type Score  = Integer

type Circle     = PointedList Marble
type Scoreboard = Map Player Score

data GameState = GameState {
        numPlayers :: Player
      , maxMarble  :: Marble
      , circle     :: Circle
      , currPlayer :: Player
      , scoreboard :: Scoreboard
    }
    deriving Generic
instance NFData (PointedList Marble) where
   rnf pl = (_reversedPrefix pl) `deepseq` (_focus pl) `deepseq` (_suffix pl) `deepseq` ()

instance NFData GameState

-- input looks like this:
-- 439 players; last marble is worth 7Data.List.1307 points
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
      , circle     = singleton 0
      , currPlayer = 0
      , scoreboard = empty
    }

maxScore :: Player -> Marble -> Score
maxScore p m =
    Data.Map.foldr max 0 (scoreboard finalGameState)
    where
        finalGameState = foldl' (\gs nextMarble ->
            let
                output = if nextMarble `mod` 1000 == 0 then
                            traceShow nextMarble (force (placeMarbles nextMarble gs))
                         else
                            force (placeMarbles nextMarble gs)
            in
                output) (startState p m) [23,46..m]

-- place the marbles for the 22 "normal" steps and the one special one,
-- update the game state based on it
placeMarbles :: Marble -> GameState -> GameState
placeMarbles i gs =
    gs {
        circle     = updateCircle23 insertedCircle
      , currPlayer = nextPlayer
      , scoreboard = updateScoreboard (scoreboard gs) (currPlayer gs) (insertedCircle) i
        }
    where
        nextPlayer =
            (currPlayer gs + 23) `mod` (numPlayers gs)
        insertedCircle =
            Data.List.foldr (flip updateCircle) (circle gs) $ reverse [i-22..i-1]

-- normal update case, add new marble in the correct position (2 to the right
-- of the current marble)
updateCircle :: Circle -> Marble -> Circle
updateCircle circle i =
    Data.List.PointedList.insert i (next circle)

-- remove marble 7 to the "left" of the currentMarble
updateCircle23 :: Circle -> Circle
updateCircle23 circle =
    fromJust $ deleteRight $ moveN (-7) circle

-- updated the scoreboard based on the marble to be placed and the marble 7
-- to the left
updateScoreboard :: Scoreboard -> Player -> Circle -> Marble -> Scoreboard
updateScoreboard scoreboard currPlayer circle i =
    if currPlayer `member` scoreboard then
        adjust (+ points) currPlayer scoreboard
    else
        Data.Map.insert currPlayer points scoreboard
    where
        points = i + _focus (moveN (-7) circle)

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    let
        (players, lastMarble) = parseInput content
    print $ maxScore players lastMarble
