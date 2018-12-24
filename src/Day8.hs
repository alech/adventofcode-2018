module Main where

import Control.Applicative
import Text.Trifecta
import System.Environment
import Safe (atMay)

type MetaData = Integer
data Tree =
    Node [Tree] [MetaData]
    deriving (Eq, Show)

parseTree :: Parser Tree
parseTree = do
    nrOfChildNodes      <- fromIntegral <$> integer
    nrOfMetaDataEntries <- fromIntegral <$> integer
    childNodes          <- count nrOfChildNodes parseTree
    metaDataEntries     <- count nrOfMetaDataEntries integer
    pure $ Node childNodes metaDataEntries

-- first part, just add up the metadata, i.e. the sum of the metadata
-- entries of the root plus the sum of the added up metadata entries
-- of the children
addUpMetaData :: Tree -> Integer
addUpMetaData (Node ts mds) = sum mds + sum (addUpMetaData <$> ts)

-- second part, trees without children have value sum of metadata
-- trees with children have value sum of values of (1-based) indices
-- subtrees where indices come from the metadata.
valueOfTree :: Tree -> Integer
valueOfTree (Node [] mds) =
    sum mds
valueOfTree (Node ts mds) =
    foldr (\md sum -> case ts `atMay` fromIntegral (md - 1) of
                            Nothing -> sum
                            Just t  -> sum + valueOfTree t) 0 mds

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    let
        tree = parseString parseTree mempty content
    print tree
    print $ addUpMetaData <$> tree
    print $ valueOfTree <$> tree
