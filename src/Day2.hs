module Main where

import System.Environment

-- maps characters 'a' to 'z' to how often they appear in the string
charCount :: String -> [Int]
charCount s =
    (\c -> length $ filter (==c) s) <$> ['a'..'z']

numberOfListThatContain :: Int -> [[Int]] -> Int
numberOfListThatContain n ll = length $ filter (\l -> n `elem` l) ll

allCombinations :: [a] -> [(a, a)]
allCombinations xs = [(x, y) | x <- xs, y <- xs]

commonChars :: String -> String -> String
commonChars s1 s2 = map fst $ filter (uncurry (==)) $ zip s1 s2

main :: IO ()
main = do
    [f]     <- getArgs
    content <- lines <$> readFile f
    print $ (twoCount content) * (threeCount content)
    -- one difference produces 25 common chars, since box IDs have 26
    print $ filter (\s -> length s == 25) (uncurry commonChars <$> allCombinations content)
    where
        twoCount   c = numberOfListThatContain 2 (charCount <$> c)
        threeCount c = numberOfListThatContain 3 (charCount <$> c)
