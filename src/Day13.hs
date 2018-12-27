module Main where

import System.Environment
import Data.Map (Map(..), (!?), keys, fromList)
import Data.Maybe
import Data.List (intercalate, nubBy, (\\))
import Debug.Trace

type Coordinate = (Integer, Integer)
type TrackMap = Map Coordinate TrackTile

data TrackTile =
    Vertical
  | Horizontal
  | SlashCurve      -- TODO would be interesting to find a better
  | BackslashCurve  -- "topological" description of the two curves
  | Intersection
  deriving (Show, Eq)

data Cart = Cart {
    coordinate :: Coordinate
  , direction  :: Direction
  , nextTurn   :: Turn
    }
    deriving (Show, Eq)

data Turn =
    LeftTurn
  | GoStraight
  | RightTurn
  deriving (Show, Eq)

data Direction =
    North
  | South
  | East
  | West
  deriving (Show, Eq)

linesToCoordinatesWithChars :: [String] -> [(Coordinate, Char)]
linesToCoordinatesWithChars content =
    concat $ (\(y, s) -> (\(x, c) -> ((x, y), c)) <$> zip [0..] s) <$> zip [0..] content

parseMap :: [(Coordinate, Char)] -> TrackMap
parseMap =
    fromList . Data.Maybe.mapMaybe (\(coord, ch) ->
            case trackTileFromChar ch of
                Just t  -> Just (coord, t)
                Nothing -> Nothing
    ) 

parseCarts :: [(Coordinate, Char)] -> [Cart]
parseCarts input =
    (\(coord, ch) ->
        Cart {
            coordinate = coord
          , direction  = charToDirection ch
          , nextTurn   = LeftTurn
        }
    ) <$> Prelude.filter (\(coord, ch) -> ch `elem` "><^v") input
    where
        charToDirection :: Char -> Direction
        charToDirection '>' = East
        charToDirection '<' = West
        charToDirection '^' = North
        charToDirection 'v' = South

trackTileFromChar :: Char -> Maybe TrackTile
trackTileFromChar '-'  = Just Horizontal
trackTileFromChar '|'  = Just Vertical
trackTileFromChar '/'  = Just SlashCurve
trackTileFromChar '\\' = Just BackslashCurve
trackTileFromChar '+'  = Just Intersection
trackTileFromChar '<'  = Just Horizontal -- cart on track, but track is
trackTileFromChar '>'  = Just Horizontal -- still horizontal
trackTileFromChar 'v'  = Just Vertical   -- samesame for vertical
trackTileFromChar '^'  = Just Vertical
trackTileFromChar _    = Nothing

moveCart :: TrackMap -> Cart -> Cart
moveCart trackMap cart =
    Cart {
        coordinate = newCoordinate
      , direction  = newDirection
      , nextTurn   = newNextTurn
    }
    where
        newCoordinate =
            case direction cart of
                East  -> (x + 1, y)
                West  -> (x - 1, y)
                North -> (x, y - 1)
                South -> (x, y + 1)
            where
                x = fst $ coordinate cart
                y = snd $ coordinate cart
        newDirection =
            case trackMap !? newCoordinate of
                Just SlashCurve     ->
                    directionAfterCurve cart True
                Just BackslashCurve ->
                    directionAfterCurve cart False
                Just Intersection ->
                    directionAfterIntersection cart
                _                 ->
                    direction cart
        newNextTurn =
            if trackMap !? newCoordinate == Just Intersection then
                case (nextTurn cart) of
                    LeftTurn   -> GoStraight
                    GoStraight -> RightTurn
                    RightTurn  -> LeftTurn
            else
                nextTurn cart

directionAfterIntersection :: Cart -> Direction
directionAfterIntersection cart =
    newDirection (direction cart) (nextTurn cart)
    where
        newDirection East  LeftTurn   = North
        newDirection East  RightTurn  = South
        newDirection West  LeftTurn   = South
        newDirection West  RightTurn  = North
        newDirection South LeftTurn   = East
        newDirection South RightTurn  = West
        newDirection North LeftTurn   = West
        newDirection North RightTurn  = East
        newDirection dir   GoStraight = dir -- continue as before

directionAfterCurve :: Cart -> Bool -> Direction
directionAfterCurve cart isSlashCurve =
    if isSlashCurve then
        case direction cart of
            North -> East
            South -> West
            East  -> North
            West  -> South
    else
        case direction cart of
            North -> West
            South -> East
            East  -> South
            West  -> North

drawTrack :: TrackMap -> [Cart] -> String
drawTrack trackMap carts =
    intercalate "\n" $
    (\y ->
        (\x ->
            if cartAtPosition carts (x, y) then
                cartChar carts (x, y)
            else
                case trackMap !? (x, y) of
                    Just t     -> trackChar t
                    Nothing    -> ' '
        )
        <$>
        [0..maxX]
    )
    <$>
    [0..maxY]
    where
        maxY = maximum $ snd <$> keys trackMap
        maxX = maximum $ fst <$> keys trackMap
        cartAtPosition carts coord =
            coord `elem` (coordinate <$> carts)
        cartChar carts coord =
            case direction $ head $ Prelude.filter (\cart -> coordinate cart == coord) carts of
                South -> 'v'
                North -> '^'
                East  -> '>'
                West  -> '<'
        trackChar Horizontal     = '-'
        trackChar Vertical       = '|'
        trackChar Intersection   = '+'
        trackChar SlashCurve     = '/'
        trackChar BackslashCurve = '\\'

cartsHaveCollided :: [Cart] -> Bool
cartsHaveCollided carts =
    length carts /= length (nubBy sameCoordinates carts)

sameCoordinates :: Cart -> Cart -> Bool
sameCoordinates c1 c2 =
    coordinate c1 == coordinate c2

collisionCoordinates :: [Cart] -> [Coordinate]
collisionCoordinates carts =
    coordinate <$> carts \\ (nubBy sameCoordinates carts)

-- move carts and potentially remove collided carts
moveAndRemoveCarts :: TrackMap -> [Cart] -> [Cart]
moveAndRemoveCarts trackMap carts =
    if cartsHaveCollided newCarts then
        filter (\c -> coordinate c `notElem` collisionCoordinates newCarts) newCarts
    else
        newCarts
    where
        newCarts = (moveCart trackMap) <$> carts

main :: IO ()
main = do
    [f]     <- getArgs
    content <- lines <$> readFile f
    let
        cwc = linesToCoordinatesWithChars content
        trackMap       = parseMap cwc
        carts          = parseCarts cwc
        collisionState = head $ dropWhile (not . cartsHaveCollided) $ iterate (fmap (moveCart trackMap)) carts
        oneCarLeftState = head $ dropWhile (\cs -> length cs > 1) $ iterate (\c -> traceShow c (moveAndRemoveCarts trackMap c)) carts
    print $ collisionCoordinates collisionState
    print $ oneCarLeftState
