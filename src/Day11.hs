module Main where

import System.Environment
import Data.List (maximumBy)
import Debug.Trace

type Coordinate = (Integer, Integer)

powerLevel :: Integer -> Coordinate -> Integer
powerLevel gridSerialNumber (x, y) =
    hundredsDigit (rackId * (rackId * y + gridSerialNumber)) - 5
    where
        rackId = x + 10
        hundredsDigit x =
            (x `div` 100) `mod` 10

-- create a three by three square of coordinates starting at (x, y)
threeByThreeSquare :: Coordinate -> [Coordinate]
threeByThreeSquare (startX, startY) =
    [(x, y) | x <- [startX..startX+2], y <- [startY..startY+2]]

powerLevelForThreeByThreeSquare :: Integer -> Coordinate -> Integer
powerLevelForThreeByThreeSquare gridSerialNumber (x, y) =
    sum $ powerLevel gridSerialNumber <$> threeByThreeSquare (x, y)

powerLevelForNByNSquare :: Integer -> Integer -> Coordinate -> Integer
powerLevelForNByNSquare n gridSerialNumber (x, y) =
    sum $ powerLevel gridSerialNumber <$> nByNSquare n (x, y)

allPossibleThreeByThreeSquaresStartCoords :: [Coordinate]
allPossibleThreeByThreeSquaresStartCoords =
    [(x, y) | x <- [1..298], y <- [1..298]]

-- create a n x n square of coordinates starting at (x, y)
nByNSquare :: Integer -> Coordinate -> [Coordinate]
nByNSquare n (startX, startY) =
    [(x, y) | x <- [startX..startX+(n-1)],
              y <- [startY..startY+(n-1)]]

allPossibleNByNSquaresStartCoords :: Integer -> [Coordinate]
allPossibleNByNSquaresStartCoords n =
    [(x, y) | x <- [1..300-(n-1)],
              y <- [1..300-(n-1)]]

main :: IO ()
main = do
    [f]     <- getArgs
    gridSerialNumber <- read <$> readFile f :: IO Integer
    let
        maxNSquare n = 
            traceShow (n, maxN) maxN
            where
                maxN =
                    maximumBy (\(_, a) (_, b) -> compare a b) $
                              (\c -> (c, powerLevelForNByNSquare n gridSerialNumber c)) <$> allPossibleNByNSquaresStartCoords n
    print $ maxNSquare 3
    print $ maximumBy (\(_, (_, a)) (_, (_, b)) -> compare a b) $
        (\n -> (n, maxNSquare n)) <$> [1..300]
