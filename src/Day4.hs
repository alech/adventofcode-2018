module Main where

import System.Environment
import Data.Sort
import Data.Map hiding (null, foldl, drop)
import Data.Maybe
import Debug.Trace

type GuardId = Integer
type GuardShiftMap = Map GuardId GuardShiftEvents

data GuardShiftEvents =
    GuardShiftEvents {
        fallsAsleepMinutes :: [Integer]
      , wakesUpMinutes     :: [Integer]
    }
    deriving (Show, Eq)

-- needed for temporary keeping around so we can work on a line-by-line
-- basis
data CurrentInfo =
    CurrentInfo {
        guardId            :: Maybe GuardId
      , ciFallsAsleepMinutes :: [Integer]
      , ciWakesUpMinutes     :: [Integer]
    }
    deriving (Eq, Show)

fourthWord :: String -> String
fourthWord s = trace ("fourthWord for " ++ s) $ (words s) !! 3

minuteFromLogLine :: String -> Integer
minuteFromLogLine s = read (s !! 15 : [s !! 16])

emptyCi :: CurrentInfo
emptyCi = CurrentInfo Nothing [] []

guardIdFromLogLine :: String -> Maybe Integer
guardIdFromLogLine s =
    Just $ read (drop 1 $ (fourthWord s))

mergeEvents :: GuardShiftEvents -> GuardShiftEvents -> GuardShiftEvents
mergeEvents gse1 gse2 =
    GuardShiftEvents (fallsAsleepMinutes gse2 ++ fallsAsleepMinutes gse1) (wakesUpMinutes gse2 ++ wakesUpMinutes gse1)

parseIntoGuardShiftMap :: [String] -> GuardShiftMap
parseIntoGuardShiftMap log =
    snd $ foldl (\(ci, m) a ->
            case head (trace a $ fourthWord a) of
                '#' ->
                    if isJust (guardId ci) && (not . null) (ciFallsAsleepMinutes ci) && (not . null) (ciWakesUpMinutes ci) then
                        (CurrentInfo (guardIdFromLogLine a) [] [],
                         traceShow ci $ insertWith mergeEvents
                            (fromJust $ guardId ci)
                            (GuardShiftEvents (ciFallsAsleepMinutes ci) (ciWakesUpMinutes ci))
                            m
                        )
                    else
                        (CurrentInfo (guardIdFromLogLine a) [] [], m)
                'a' -> -- "asleep"
                    traceShow ci $ (ci {
                        ciFallsAsleepMinutes = (ciFallsAsleepMinutes ci) ++
                            [minuteFromLogLine a] 
                        },
                    m)
                'u' -> -- "up"
                    traceShow ci $ (ci { 
                        ciWakesUpMinutes = (ciWakesUpMinutes ci) ++
                            [minuteFromLogLine a]
                        },
                    m)
        ) (emptyCi, empty) log

totalSleepTime :: GuardShiftMap -> [(GuardId, Integer)]
totalSleepTime =
    Data.Map.foldrWithKey (\gid gse b ->
        (gid, 
        sum $ zipWith (-) (wakesUpMinutes gse) (fallsAsleepMinutes gse)
        ) : b) []

guardAsleepByMinute :: GuardId -> GuardShiftMap -> [(Integer, Maybe Integer)]
guardAsleepByMinute gid gsm =
    (\m ->
        (m,
        (\gse ->
            Prelude.foldr (\(f, w) b ->
                if m >= f && m < w then
                    b + 1
                else
                    b
            ) 0 (zip (fallsAsleepMinutes gse) (wakesUpMinutes gse))
        ) <$> (Data.Map.lookup gid gsm)
        )
    ) <$> [0..59]

sleepiestMinute :: [(Integer, Maybe Integer)] -> Integer
sleepiestMinute =
    fst . head . sortBy (\(_, a) (_, b) -> b `compare` a)

guardIdOfSleepiestGuard :: [String] -> GuardId
guardIdOfSleepiestGuard content =
    fst $ head $ sortBy (\(_, a) (_, b) -> b `compare` a) $ totalSleepTime $ parseIntoGuardShiftMap content

main :: IO ()
main = do
    [f]     <- getArgs
    content <- sort <$> lines <$> readFile f
    --print $ content
    print $ guardIdOfSleepiestGuard content
    print $ sleepiestMinute $ guardAsleepByMinute
                                (guardIdOfSleepiestGuard content)
                                (parseIntoGuardShiftMap content)
    return ()
