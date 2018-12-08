module Main where

import System.Environment
import Data.List (sort)

type DependencyList = [(Char, Char)]

-- parse input into a list of dependencies, e.g.
-- "Step C must be finished before step A can begin."
-- gets turned into ('A', 'C')
dependenciesFromInput :: String -> DependencyList
dependenciesFromInput content =
    (\s -> (head (words s !! 7), head (words s !! 1))) <$> lines content
    
order :: DependencyList -> String
order ds =
    go orderedChars ds
    where
        go :: String -> DependencyList -> String
        go "" _     = ""
        go toTry ds =
            -- find a char without dependencies
            case dropWhile (\x -> x `elem` (fst <$> ds)) toTry of
                ""     -> ""
                -- add current char to result and continue without that char
                -- in the toTry list and the dependencies that depend on that
                -- char being finished removed
                (x:xs) -> x : go (filter (/=x) toTry) (filter (\dep -> snd dep /= x) ds)
        orderedChars =
            sort $ (fst <$> ds) ++ (snd <$> ds)

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    putStrLn $ order (dependenciesFromInput content)
