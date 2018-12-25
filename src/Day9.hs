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
--import Debug.Trace

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
                output = force (placeMarble nextMarble gs)
            in
                output) (startState p m) [1..m]

placeMarble :: Marble -> GameState -> GameState
placeMarble i gs
    | (i `mod` 23) == 0  =
        gs {
            circle     = updateCircle23 (circle gs)
          , currPlayer = nextPlayer
          , scoreboard = updateScoreboard (scoreboard gs) (currPlayer gs) (circle gs) i
        }
    -- "normal" case, update circle
    | otherwise          =
        gs {
            circle = updateCircle (circle gs) i
          , currPlayer = nextPlayer
        }
    where
        nextPlayer = (currPlayer gs + 1) `mod` (numPlayers gs)

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
