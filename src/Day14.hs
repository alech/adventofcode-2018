{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Environment
import Data.Int
import Control.Monad.State.Strict
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.IntMap as IM
import Data.Maybe

data RecipeState = RecipeState {
    recipes :: IM.IntMap Int8
  , firstElfIndex  :: Int
  , secondElfIndex :: Int
  , numRecipes :: Int
  }
  deriving (Show, Eq, Generic)
instance NFData RecipeState

playGame :: Int -> State RecipeState [Int8]
playGame 0 = do
    RecipeState r _ _ nr <- get
    --return $ mapMaybe (\i -> IM.lookup i r) [nr-10..nr]
    return $ snd <$> IM.toAscList r
playGame x = do
    recipeState <- get
    put $ force $ RecipeState {
        recipes = newRecipes recipeState
      , firstElfIndex = newIndex (firstElfIndex recipeState) recipeState
      , secondElfIndex = newIndex (secondElfIndex recipeState) recipeState
      , numRecipes = newNumRecipes recipeState
    }
    playGame (x - numsAdded recipeState)
    where
        newRecipeNumber rs =
              (recipes rs IM.! firstElfIndex rs)
            + (recipes rs IM.! secondElfIndex rs)
        newRecipes rs =
            if newRecipeNumber rs >= 10 then
                IM.insert
                (numRecipes rs + 1) (newRecipeNumber rs `mod` 10)
                (IM.insert (numRecipes rs) (newRecipeNumber rs `div` 10) (recipes rs))
            else
                IM.insert
                (numRecipes rs) (newRecipeNumber rs) (recipes rs)
        newNumRecipes rs = numRecipes rs + numsAdded rs
        numsAdded rs = if newRecipeNumber rs >= 10 then 2 else 1
        newIndex :: Int -> RecipeState -> Int
        newIndex i rs =
            (i + 1 + fromIntegral (newRecipes rs IM.! i)) `mod` (newNumRecipes rs)

initialRecipeState :: RecipeState
initialRecipeState = RecipeState (IM.fromList [(0,3), (1,7)]) 0 1 2

main :: IO ()
main = do
    [f]     <- getArgs
    input <- read <$> readFile f :: IO Int
    putStrLn $ concatMap show $ take 10 $ drop input $ evalState (playGame (input + 10)) initialRecipeState
    --print $ drop input $ recipes $ head $ dropWhile (\s -> length (recipes s) < input + 10) $ iterate nextRecipeState initialRecipeState
    return ()
