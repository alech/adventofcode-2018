module Main where

import System.Environment
import Data.Map (Map(..), foldr, empty, adjust, insert, member)
import Data.Maybe (fromJust)
import Data.List
--import Debug.Trace

type Player = Integer
type Marble = Integer
type Score  = Integer

type Circle     = [Marble]
type Scoreboard = Map Player Score

-- input looks like this:
-- 439 players; last marble is worth 71307 points
parseInput :: String -> (Player, Marble)
parseInput s =
    (nthWord 0, nthWord 6)
    where
        nthWord i = read (words s !! i)

maxScore :: Player -> Marble -> Score
maxScore p m =
    -- start playing with a 0 marble in the circle, current marble is 0,
    -- current player is 0, scoreboard is empty and marble to be placed next is
    -- 1
    play p m [0] 0 0 empty 1

play :: Player -> Marble -> Circle -> Marble -> Player -> Scoreboard -> Marble -> Score
play numPlayers maxMarble circle currMarble currPlayer scoreboard i
    -- we are done, let's calculate the max score from the scoreboard
    | i == maxMarble + 1 = Data.Map.foldr max 0 scoreboard
    -- special case, need to update the circle, the current marble and
    -- the scoreboard based on the mod 23 rule
    | (i `mod` 23) == 0  =
        play
            numPlayers
            maxMarble
            --(trace ("new circ23: " ++ show (updateCircle23 circle currMarble)) (updateCircle23 circle currMarble))
            (updateCircle23 circle currMarble)
            (updateCurrMarble23 circle currMarble)
            nextPlayer
            (updateScoreboard scoreboard currPlayer circle i currMarble)
            (i + 1)
    -- "normal" case, update circle
    | otherwise          =
        play
            numPlayers
            maxMarble
            --(trace ("new circle: " ++ show (updateCircle circle currMarble i)) (updateCircle circle currMarble i))
            (updateCircle circle currMarble i)
            i
            nextPlayer
            scoreboard
            (i + 1)
    where
        nextPlayer = (currPlayer + 1) `mod` numPlayers

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
