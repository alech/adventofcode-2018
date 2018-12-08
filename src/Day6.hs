module Main where

import System.Environment
import Data.List (nub)
import Data.Sort (sort)

type Coordinate = (Integer, Integer)

-- cf. Wikipedia
distance :: Coordinate -> Coordinate -> Integer
distance (x1, y1) (x2, y2) =
    abs(x1 - x2) + abs(y1 - y2)

-- parse a string like "13, 198" into a coordinate
toCoordinate :: String -> Coordinate
toCoordinate s =
    (read $ takeWhile (/=',') s,
     read $ dropWhile (/=' ') s)

-- parse file content into a list of Coordinates
coordinates :: String -> [Coordinate]
coordinates s =
    toCoordinate <$> lines s

-- the grid seems to be square always(?)
gridSize :: [Coordinate] -> Integer
gridSize cl =
    maximum $ (fst <$> cl) ++ (snd <$> cl)

-- given input coordinates, make a grid from 0..max (maxX, maxY)
grid :: [Coordinate] -> [Coordinate]
grid cl =
    [(x, y) | y <- [0..gridSize cl], x <- [0..gridSize cl]]

-- given an input list of coordinates, compute all positions on
-- the outer border of the grid
gridBorder :: [Coordinate] -> [Coordinate]
gridBorder cl = nub $
    [(0, y)           | y <- zeroToN] ++
    [(x, 0)           | x <- zeroToN] ++
    [(gridSize cl, y) | y <- zeroToN] ++
    [(x, gridSize cl) | x <- zeroToN]
    where
        zeroToN = [0..gridSize cl]

-- Given a coordinate and a list of coordinate, determine the
-- index in the list of coordinates which is closest to the
-- given coordinate. If there is a tie, Nothing is returned.
closestIndex :: [Coordinate] -> Coordinate -> Maybe Int
closestIndex cl c =
    case length filteredForMinD of
        1 -> Just (fst $ head filteredForMinD)
        _ -> Nothing
    where 
        minD =
            minimum (distance c <$> cl)
        filteredForMinD =
            filter (\(_, d) -> d == minD) indexAndDistance
        indexAndDistance =
            zip [0..] (distance c <$> cl)

-- Given a list of coordinates, create a grid of closest indices
closestGrid :: [Coordinate] -> [Maybe Int]
closestGrid cl =
    closestIndex cl <$> grid cl

-- given an index of a coordinate, determine if a closest index for
-- it is on the grid border (which means that this area extends
-- infinitely and will be ignored
isOnGridBorder :: Int -> [Coordinate] -> Bool
isOnGridBorder i cl =
    Just i `elem` (closestIndex cl <$> gridBorder cl)

-- given an index of a coordinate, determine the size on the grid
-- of closest points
finiteSizeByIndex :: Int -> [Coordinate] -> Int
finiteSizeByIndex i cl =
    length $ filter (==Just i) (closestGrid cl)

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    let coords = coordinates content in
        print $ head $ reverse $ sort $ map (\i -> finiteSizeByIndex i coords) $ filter (\i -> not $ isOnGridBorder i coords) [0..length coords-1]
    return ()
