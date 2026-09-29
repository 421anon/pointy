module Main (main) where

import Agent.Runner (TurnBudgetPlan (..), TurnBudgetStep (..), turnBudgetPlan)
import Data.List (foldl')

main :: IO ()
main = do
    stoppedWhenBudgetSpent
    pausedTimeIsNotCharged
    noticeFiresOnce
    noticeBeforeStop

budgetMicros :: Int
budgetMicros = 30 * 60 * 1000000

freshStep :: TurnBudgetStep
freshStep = TurnBudgetStep{stepChargedMicros = 0, stepPaused = False, stepWarned = False, stepStopped = False}

describe :: TurnBudgetPlan -> String
describe plan = case plan of
    BudgetContinue _ -> "continue"
    BudgetWarn _ -> "warn"
    BudgetStop _ -> "stop"

planned :: TurnBudgetPlan -> TurnBudgetStep
planned plan = case plan of
    BudgetContinue next -> next
    BudgetWarn next -> next
    BudgetStop next -> next{stepStopped = True}

outcomes :: Int -> [Int] -> [String]
outcomes budget = snd . foldl' tick (freshStep, [])
  where
    tick (step, seen) charge =
        let plan = turnBudgetPlan budget step{stepChargedMicros = stepChargedMicros step + charge}
         in (planned plan, seen ++ [describe plan])

stoppedWhenBudgetSpent :: IO ()
stoppedWhenBudgetSpent = do
    let minute = 60 * 1000000
        seen = outcomes budgetMicros (replicate 31 minute)
    assertEqual "the turn is not stopped before the budget is spent" (replicate 30 "continue") (map (const "continue") (take 30 seen))
    assertEqual "the overlap notice does not stop the turn" 1 (length (filter (== "warn") seen))
    assertEqual "the 31st minute stops it" "stop" (last seen)
    assertEqual "stopping repeats while the budget stays spent" "stop" (last (outcomes budgetMicros (replicate 60 minute)))

pausedTimeIsNotCharged :: IO ()
pausedTimeIsNotCharged = do
    let minute = 60 * 1000000
        active = foldl' (\step micros -> planned (turnBudgetPlan budgetMicros step{stepChargedMicros = stepChargedMicros step + micros})) freshStep (replicate 29 minute)
        pausedInterval = active{stepPaused = True}
        afterPause = planned (turnBudgetPlan budgetMicros pausedInterval)
        afterMore = afterPause{stepChargedMicros = stepChargedMicros afterPause + 2 * minute}
    assertEqual "a paused interval charges nothing" (stepChargedMicros active) (stepChargedMicros afterPause)
    assertEqual "resuming clears the pause" False (stepPaused afterPause)
    assertEqual "ten paused minutes do not count as active time" "continue" (describe (turnBudgetPlan budgetMicros afterPause))
    assertEqual "two active minutes still stop the turn" "stop" (describe (turnBudgetPlan budgetMicros afterMore))

noticeFiresOnce :: IO ()
noticeFiresOnce = do
    let minute = 60 * 1000000
        seen = outcomes budgetMicros (replicate 30 minute)
    assertEqual "the notice fires once" 1 (length (filter (== "warn") seen))
    assertEqual "the notice fires after three quarters of the budget" 22 (length (takeWhile (/= "warn") seen))

noticeBeforeStop :: IO ()
noticeBeforeStop = do
    let seen = outcomes budgetMicros [budgetMicros * 3 `div` 4 + 1]
    assertEqual "crossing the notice level warns first" ["warn"] (filter (== "warn") seen)
    assertEqual "crossing the budget stops" "stop" (describe (turnBudgetPlan budgetMicros freshStep{stepChargedMicros = budgetMicros}))

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
