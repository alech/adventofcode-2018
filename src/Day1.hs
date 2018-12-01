module Main where

import System.Environment
import Data.Monoid (Sum(..))
import Data.List (nub, inits)
import Data.Set (insert, Set(..), empty)

stripLeadingPlus :: String -> String
stripLeadingPlus ('+':s) = s
stripLeadingPlus x       = x

-- takes ~3 minutes on my input
findDuplicateSum :: Integer -> Int -> Set Integer -> [Integer] -> Integer
findDuplicateSum currentSum i s l =
    if nextSum `elem` s then
        nextSum 
    else
        findDuplicateSum nextSum (i + 1) (insert nextSum s) l
    where
        nextSum = currentSum + (l !! i)

--hasDuplicates :: Eq a => [a] -> Bool
--hasDuplicates l = nub l /= l

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    print $ foldMap (Sum . read . stripLeadingPlus) (lines content)
    -- two attemts that were too expensive
    --print $ head $ dropWhile (not . hasDuplicates) (partialSums $ repeatingList content)
    --print $ head $ dropWhile (not . hasDuplicates) (inits ((\n -> sumUpToN n (repeatingList content)) <$> [1..]))
    print $ findDuplicateSum 0 0 empty (repeatingList content)
    where
        --sumUpToN n l = sum $ take n l
        --partialSums l = (\x -> (\i -> sum $ take i l) <$> [1..x]) <$> [1..]
        repeatingList c = (concat . repeat) $ (\x -> read $ stripLeadingPlus x :: Integer) <$> lines c
