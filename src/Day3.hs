module Main where

import System.Environment
import Data.List.Split (splitOn)
import Data.Set hiding (take, partition, filter)
import Data.List (partition)

claimToOccupyingCoordinates :: String -> [(Integer, Integer)]
claimToOccupyingCoordinates s =
    [(x, y) | x <- [startX..startX+lengthX-1],
              y <- [startY..startY+lengthY-1]]
    where
        splitString  = splitOn " " s
        coordsString = splitString !! 2
        coords = (\s -> read s :: Integer) <$> (splitOn "," $ take ((length coordsString) - 1) coordsString)
        startX = coords !! 0
        startY = coords !! 1
        lengths = (\s -> read s :: Integer) <$> (splitOn "x" $ splitString !! 3)
        lengthX = lengths !! 0
        lengthY = lengths !! 1

overLappingCount :: [String] -> Int
overLappingCount xs =
    go empty empty xs
    where
        go :: Set (Integer, Integer) -> Set (Integer, Integer) -> [String] -> Int
        go _ overLapping [] = length overLapping
        go notOverLapping overLapping (x:xs) =
            go (notOverLapping `union` (fromList n)) (overLapping `union` (fromList o)) xs
            where
                (o, n) = partition (\e -> e `member` notOverLapping) (claimToOccupyingCoordinates x)

overlaps :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
overlaps x y =
    not $ Data.Set.null $ intersection (fromList x) (fromList y)

main :: IO ()
main = do
    [f]     <- getArgs
    content <- lines <$> readFile f
    print $ overLappingCount content
    print $ filter (\(_, b) -> b) $ zip [1..] (fmap and $ (fmap . fmap) (\(x, y) -> not $ overlaps x y) (allCombinations (claimToOccupyingCoordinates <$> content)))
    where
        allCombinations claims = (fmap . fmap) (\(a, b) -> (claims !! (a-1), claims !! (b-1))) ((\x -> [(x, y) | y <- [1..length claims], y /= x]) <$> [1..length claims])
