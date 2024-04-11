module Exercise02 where

import Data.List
import Data.Ord
import Data.Maybe

{-H2.1a)-}
twoThirdsAverageWinners :: [(String, Int)] -> [String]
twoThirdsAverageWinners gs = [name | (name, x) <- gs, abs (x - a) == abs (best - a)]
    where a = 2 * (foldr (\(_,x) acc -> x + acc) 0 gs `div` length gs) `div` 3
          best = foldr (\(_, x) curBest -> if abs (x - a) < abs (curBest - a) then x else curBest) (snd (head gs)) gs

twoThirdsAverageWinners2 :: [(String, Int)] -> [String]
twoThirdsAverageWinners2 gs = [n | (n, g) <- gs, bidDistance g == bestBidDistance]
  where
    twoThirdsAverage = (2 * sum [g | (_, g) <- gs])  `div` (3 * length gs)
    bidDistance b = abs (b - twoThirdsAverage)
    bestBidDistance = minimum [bidDistance g | (_, g) <- gs]

prop_twoThirds :: [(String, Int)] -> Bool 
prop_twoThirds gs = twoThirdsAverageWinners gs == twoThirdsAverageWinners2 gs

{-H2.1b)-}
lowestUniqueBidder :: [(String, Int)] -> String
lowestUniqueBidder bs = if null m then "Nobody" else fst $ fromJust $ find (\(a, b) -> b == minimum m) bs
    where m = concat $ filter (\x -> length x == 1) $ group $ sort $ map snd bs

{-H2-}

-- returns the shortest list in a list of lists
shortest :: [[Int]] -> [Int]
shortest = minimumBy (comparing length)

-- returns the set of all players in a tournament
players :: [[Int]] -> [Int]
players tournament = [1..length tournament]

-- returns the dominion of player i
dominion :: [[Int]] -> Int -> [Int]
dominion tournament i = tournament !! (i - 1)


{-H2.2a)-}
dominators :: [[Int]] -> Int -> [Int]
dominators tournament i = map fst $ filter (\(a, b) -> i `elem` b) $ zip (players tournament) tournament

{-H2.2b)-}
covers :: [[Int]] -> Int -> Int-> Bool
covers tournament i j = isSubsequenceOf (sort (dominion tournament j)) (sort (dominion tournament i)) 

{-2.2c)-}
dominant :: [[Int]] -> [Int] -> Bool
dominant _ [] = False
dominant tournament xs = undefined
    where ys = filter (\(p, _) -> p `notElem` xs) $ zip (players tournament) tournament

{-WETT-}

{-H2.2d)-}
copeland :: [[Int]] -> [Int]
copeland t = map fst $ filter (\(_, x) -> length x == max) tneu
    where tneu = zip [1..] t
          max = length (maximum t)

{-H2.2e)-}
uncoveredSet :: [[Int]] -> [Int]
uncoveredSet tournament = undefined
       
{-H2.2f)-} 
topCycle :: [[Int]] -> [Int]
topCycle tournament = undefined

{-TTEW-}

