module Main where

import System.Environment
import Data.Map (lookup, fromList, Map(..))
import Text.Trifecta
import Data.Maybe
import Data.List (intercalate)

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
    (replicate 20 '.') ++ initialState ++ (replicate 1000 '.')

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

-- determined by looking at the output of the first ~150 generations
-- and their scores, patterns just moves on to the right every generation
-- and thus increases by 52 each time.
scoreForHighGenerations i = (27820-500*52) + 52*i

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    let
        parseResult = parseString parseInput mempty content
        (initial, ruleMap) = (\(Success x) -> x) parseResult
        area = initialPotArea initial
        finalState = last $ take 21 (iterate (spread ruleMap) area)
    putStrLn $ intercalate "\n" $ take 151 $ map (\(i, a) -> show i ++ ": " ++ a ++ ": " ++ show (score a)) $ zip [1..] (iterate (spread ruleMap) area)
    print finalState
    print $ score finalState
    print $ scoreForHighGenerations 50000000001
