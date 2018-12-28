module Main where

import System.Environment
import Data.Int

data State = State {
    recipes :: [Int8]
  , firstElfIndex  :: Int
  , secondElfIndex :: Int
  }
  deriving (Show, Eq)

nextState :: State -> State
nextState state =
    State {
        recipes = newRecipes
      , firstElfIndex = newIndex (firstElfIndex state)
      , secondElfIndex = newIndex (secondElfIndex state)
    }
    where
        newRecipeNumber =
              (recipes state !! firstElfIndex state)
            + (recipes state !! secondElfIndex state)
        newRecipes = recipes state ++
            if newRecipeNumber >= 10 then
                [newRecipeNumber `div` 10, newRecipeNumber `mod` 10]
            else
                [newRecipeNumber]
        newIndex :: Int -> Int
        newIndex i =
            (i + 1 + fromIntegral (newRecipes !! i)) `mod` length newRecipes

initialState :: State
initialState = State [3, 7] 0 1

main :: IO ()
main = do
    [f]     <- getArgs
    input <- read <$> readFile f :: IO Int
    print $ drop input $ recipes $ head $ dropWhile (\s -> length (recipes s) < input + 10) $ iterate nextState initialState
    return ()
