module Main where

import System.Environment
import Data.List (maximumBy)

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

allPossibleThreeByThreeSquaresStartCoords :: [Coordinate]
allPossibleThreeByThreeSquaresStartCoords =
    [(x, y) | x <- [1..298], y <- [1..298]]

main :: IO ()
main = do
    [f]     <- getArgs
    gridSerialNumber <- read <$> readFile f :: IO Integer
    print $ maximumBy (\(_, a) (_, b) -> compare a b) $ (\c -> (c, powerLevelForThreeByThreeSquare gridSerialNumber c)) <$> allPossibleThreeByThreeSquaresStartCoords
