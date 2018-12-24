module Main where

import System.Environment
import Data.List (sort, nub, intercalate)
import Data.Maybe (mapMaybe)
import Data.Char (ord)

type Task = Char
type DependencyList = [(Task, Task)]
type Second    = Integer -- seconds remaining/taken
type Done      = String
type Possibilities = String -- all possible tasks

data WorkerState =
      Idle
    | WorkingOn Task Second
    deriving (Show, Eq)

data SystemState =
    SystemState Second [WorkerState] Done Possibilities DependencyList
    deriving (Show, Eq)

-- parse input into a list of dependencies, e.g.
-- "Step C must be finished before step A can begin."
-- gets turned into ('A', 'C')
dependenciesFromInput :: String -> DependencyList
dependenciesFromInput content =
    (\s -> (head (words s !! 7), head (words s !! 1))) <$> lines content
    
order :: DependencyList -> String
order ds =
    go (orderedChars ds) ds
    where
        go :: String -> DependencyList -> String
        go "" _     = ""
        go toTry ds =
            -- find a char without dependencies
            case dropWhile (\x -> x `elem` (fst <$> ds)) toTry of
                ""     -> ""
                -- add current char to result and continue without that char
                -- in the toTry list and the dependencies that depend on that
                -- char being finished removed
                (x:xs) -> x : go (filter (/=x) toTry) (filter (\dep -> snd dep /= x) ds)

orderedChars :: DependencyList -> Possibilities
orderedChars ds =
    nub $ sort $ (fst <$> ds) ++ (snd <$> ds)

-- move one step in the system, i.e. from one line in the table to the next.
-- For that, we count up the second, we determine if any workers can take
-- on new tasks, we add task output to the done list if a worker is done and
-- create a new dependency list
oneStepInSystem :: SystemState -> SystemState
oneStepInSystem (SystemState s workers done ps depList) =
    SystemState (s+1) newWorkers newDone ps newDepList
    where
        newDone     = done ++ freshlyDone
        -- freshly done are the ones whose working on second went to 0
        freshlyDone = mapMaybe (\w -> case w of
                                        WorkingOn c 0 -> Just c
                                        _             -> Nothing) workers
        -- remove the freshly done ones from the dependency list
        newDepList = filter (\d -> snd d `notElem` freshlyDone) depList
        -- new workers are a combination of occupying the idle workers
        -- and counting down the existing working workers
        newWorkers =
            occupyIdleWorkers ++ countDownWorkingWorkers

        -- a worker whose second has just turned to 0 is as good as idle
        isIdleOrFreshlyAvailable :: WorkerState -> Bool
        isIdleOrFreshlyAvailable Idle            = True
        isIdleOrFreshlyAvailable (WorkingOn _ 0) = True
        isIdleOrFreshlyAvailable _               = False

        occupyIdleWorkers =
            go (filter isIdleOrFreshlyAvailable workers) availableTasks
            where
                -- we have workers left, make them all Idle
                go ws "" = const Idle <$> ws
                -- we have tasks left but no workers
                go [] ts = []
                -- if we have a worker and a task, distribute the
                -- task
                go (w:ws) (t:ts) =
                    WorkingOn t (timeForChar t - 1) : go ws ts
        countDownWorkingWorkers =
            (\(WorkingOn c t) -> WorkingOn c (t-1)) <$> 
                filter (not . isIdleOrFreshlyAvailable) workers
        availableTasks =
            -- tasks are available if they are not already done
            -- and if they are not the first part of an entry in
            -- the new dependency list.
            -- also, no other worker can work on them at the moment
            filter (\p -> p `notElem` newDone &&
                          p `notElem` (fst <$> newDepList) && 
                          p `notElem` (mapMaybe (\w ->
                                                    case w of
                                                        Idle -> Nothing
                                                        WorkingOn c _ -> Just c) workers)) ps

-- 'A' -> 61
-- 'B' -> 62
-- ...
timeForChar :: Char -> Second
timeForChar c = fromIntegral $ 60 + (ord c - 64)

main :: IO ()
main = do
    [f]     <- getArgs
    content <- readFile f
    putStrLn $ order (dependenciesFromInput content)
    -- show the whole system state until one second before finish
    -- answer is last SystemState time value + 1
    let
        notDone (SystemState _ _ d p _) = length d /= length p
        depList = dependenciesFromInput content
        ps = orderedChars depList
        startState = SystemState (-1) (replicate 5 Idle) "" ps depList
    putStrLn $ intercalate "\n"
                           (show <$> takeWhile notDone (iterate oneStepInSystem startState))
