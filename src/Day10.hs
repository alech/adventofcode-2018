module Main where

import Control.Applicative
import Text.Trifecta
import System.Environment
import Data.List (nub, intercalate, sortBy)
import Codec.Picture

type Coordinate = (Integer, Integer)
type Velocity   = (Integer, Integer)
data PointOfLight = PointOfLight Coordinate Velocity
    deriving (Eq, Show)

parseTwoIntegers :: Parser (Integer, Integer)
parseTwoIntegers = do
    _ <- skipMany (char ' ')
    x <- integer
    _ <- char ','
    _ <- skipMany (char ' ')
    y <- integer
    pure (x, y)

parsePOL :: Parser PointOfLight
parsePOL = do
    _ <- string "position=<"
    (xPos, yPos) <- parseTwoIntegers
    _ <- string "> velocity=<"
    (xVel, yVel) <- parseTwoIntegers
    _ <- char '>'
    pure $ PointOfLight (xPos, yPos) (xVel, yVel)

polInNSeconds :: Integer -> PointOfLight -> PointOfLight
polInNSeconds n (PointOfLight (xPos, yPos) (xVel, yVel)) =
    PointOfLight (xPos + n*xVel, yPos + n*yVel) (xVel, yVel)

-- when a message appears, a lot of points will be aligned on
-- the same X coordinate, let's figure out how many are
commonXCoords :: [PointOfLight] -> Int
commonXCoords pols =
    length $ nub $ (\(PointOfLight (xPos, _) _) -> xPos) <$> pols

-- second at which number of X coordinates is minimal
-- (i.e. hopefully the time the aligned image appears)
-- given a certain maximum timeframe
minimumXCoordSecond :: [PointOfLight] -> Integer -> Integer
minimumXCoordSecond pols maxTime =
    fst $ minimumBy (\(_, a) (_, b) -> compare a b) secsAndCommonXCoords
    where
        secsAndCommonXCoords =
            zip [0..maxTime]
                (commonXCoords <$> (
                (\n -> (polInNSeconds n) <$> pols) <$> [0..maxTime]
                ))

imageFromPOLs :: [PointOfLight] -> Image Pixel8
imageFromPOLs pols =
    generateImage polImage xExtend yExtend
    where
        polCoords =
            (\(PointOfLight pos _) -> pos) <$> pols
        xMin = minimum (fst <$> polCoords)
        yMin = minimum (snd <$> polCoords)
        zeroAlignedPOLCoords =
            (\(x, y) -> (x - xMin, y - yMin)) <$> polCoords
        xExtend = fromIntegral $ maximum (fst <$> zeroAlignedPOLCoords) + 1
        yExtend = fromIntegral $ maximum (snd <$> zeroAlignedPOLCoords) + 1
        polImage x y =
            if (fromIntegral x, fromIntegral y) `elem` zeroAlignedPOLCoords then
                0   -- black if we have a PointOfLight
            else
                255 -- white otherwise

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    let
        pols = foldr (\a b -> case a of
                                Success p -> p : b
                                _         -> b) [] $
               parseString parsePOL mempty <$> lines content
        minSec = minimumXCoordSecond pols 100000
    --print pols 
    print minSec
    saveBmpImage "message.bmp" (ImageY8 $ imageFromPOLs (polInNSeconds minSec <$> pols))
