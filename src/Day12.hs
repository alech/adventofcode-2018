module Main where

import System.Environment
import Data.Map (lookup, fromList, Map(..))
import Text.Trifecta
import Data.Maybe

type InitialState = String
type RuleMap = Map String Char

parsePlantChar :: Parser Char
parsePlantChar =
    oneOf ".#"

parseRule :: Parser (String, Char)
parseRule = do
    from <- many parsePlantChar
    _    <- string " => "
    to   <- parsePlantChar
    _    <- char '\n'
    pure (from, to)

parseInput :: Parser (InitialState, RuleMap)
parseInput = do
    _            <- string "initial state: "
    initialState <- many parsePlantChar
    _            <- string "\n\n"
    rules        <- many parseRule
    pure (initialState, fromList rules)

-- give a space of 20 on the left and right to allow growth
initialPotArea :: InitialState -> String
initialPotArea initialState =
    (replicate 20 '.') ++ initialState ++ (replicate 20 '.')

spread :: RuleMap -> String -> String
spread rules area =
    take 2 area ++
    ((\lv -> case lv of
            Just  c -> c
            Nothing -> '.') <$>
        (\i -> Data.Map.lookup (extractNeighborhood i) rules) <$> [2..length area-3]
    )
    ++ drop (length area - 3) area
    where
        extractNeighborhood i =
            take 5 $ drop (i-2) area

score :: String -> Integer
score s =
    sum $ (\(i, c) -> if c == '#' then i else 0) <$> zip [-20..] s

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    let
        parseResult = parseString parseInput mempty content
        (initial, ruleMap) = (\(Success x) -> x) parseResult
        area = initialPotArea initial
        finalState = last $ take 21 $ iterate (spread ruleMap) area
    print finalState
    print $ score finalState
