module Main where

import System.Environment
import Data.Maybe
import Data.Char (toLower, toUpper, isLower, isUpper)
import Debug.Trace

flipCase :: Char -> Char
flipCase c
    | isLower c = toUpper c
    | isUpper c = toLower c
    | otherwise = c

indexOfReaction :: String -> Maybe Int
indexOfReaction s =
    if null ignoreNonReactions then
        Nothing
    else
        Just $ (fst . head) ignoreNonReactions
    where
        ignoreNonReactions =
            dropWhile
                (\(i, (c1, c2)) ->
                    not (flipCase c1 == c2 || c1 == flipCase c2))
                combinationsWithIndex
        combinationsWithIndex = zip [0..] (zip s (drop 1 s))

reactPolymer :: String -> String
reactPolymer s =
    case indexOfReaction s of
        Just i ->
            --trace ("reactPolymer for s = " ++ s ++ ", i = " ++ (show i) ++ ", new string: " ++ take i s ++ drop (i + 2) s) reactPolymer (take i s ++ drop (i + 2) s)
            reactPolymer (take i s ++ drop (i + 2) s)
        Nothing ->
            s

-- possible lower case characters which have a pair in the input
containedPairs :: String -> [Char]
containedPairs content =
    filter (\c -> c `elem` content && (toUpper c) `elem` content ) ['a'..'z']

-- removes upper and lower case characters
removePair :: String -> Char -> String
removePair content c =
    filter (\ch -> ch /= c && ch /= toUpper c) content

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    --print $ content
    print $ length $ reactPolymer content
    print $ minimum $ (\co -> length $ reactPolymer co) <$> ((removePair content) <$> (containedPairs content))
