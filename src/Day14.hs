{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Environment
import Data.Int
import Control.Monad.State.Strict
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Sequence

data RecipeState = RecipeState {
    recipes :: Seq Int8
  , firstElfIndex  :: Int
  , secondElfIndex :: Int
  }
  deriving (Show, Eq, Generic)
instance NFData RecipeState

playGame :: Int -> State RecipeState (Seq Int8)
playGame 0 = do
    RecipeState l _ _ <- get
    return l
playGame x = do
    recipeState <- get
    put $ force $ RecipeState {
        recipes = newRecipes recipeState
      , firstElfIndex = newIndex (firstElfIndex recipeState) recipeState
      , secondElfIndex = newIndex (secondElfIndex recipeState) recipeState
    }
    playGame (x - 1)
    where
        newRecipeNumber rs =
              (recipes rs `index` firstElfIndex rs)
            + (recipes rs `index` secondElfIndex rs)
        newRecipes rs =
            if newRecipeNumber rs >= 10 then
                recipes rs |> newRecipeNumber rs `div` 10 |> newRecipeNumber rs `mod` 10
            else
                recipes rs |> newRecipeNumber rs
        newIndex :: Int -> RecipeState -> Int
        newIndex i rs =
            (i + 1 + fromIntegral (newRecipes rs `index` i)) `mod` Data.Sequence.length (newRecipes rs)

initialRecipeState :: RecipeState
initialRecipeState = RecipeState (fromList [3, 7]) 0 1

main :: IO ()
main = do
    [f]     <- getArgs
    input <- read <$> readFile f :: IO Int
    putStrLn $ concatMap show $ Data.Sequence.take 10 $ Data.Sequence.drop input $ evalState (playGame input) initialRecipeState
    --print $ drop input $ recipes $ head $ dropWhile (\s -> length (recipes s) < input + 10) $ iterate nextRecipeState initialRecipeState
    return ()
