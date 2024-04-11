module Exercise03 where

import Text.Printf (printf)
import Data.List

-- HA 3.1a) i
selectRow :: [[Int]] -> Int -> [Int]
selectRow = (!!)

-- HA 3.1a) ii
selectColumn :: [[Int]] -> Int -> [Int]
selectColumn xss i = transpose xss !! i

-- HA 3.1a) iii
intRoot :: Int -> Int
intRoot = floor . sqrt . fromIntegral

--return numbers in square as a list. squares are numbered  from left to right and top to bottom
--e.g. :
--[0,1,2]
--[3,4,5]
--[6,7,8]
selectSquare :: [[Int]] -> Int -> [Int]
selectSquare xss i = concat [selectColumns i xs | xs <- selectRows i]
  where
    squareSize = intRoot $ length xss
    selectRows i = take squareSize $ drop ((i `div` squareSize) * squareSize) xss
    selectColumns i xs = take squareSize $ drop ((i `mod` squareSize) * squareSize) xs

-- HA 3.1b)
isValidSubsection :: [Int] -> Bool
isValidSubsection [] = True
isValidSubsection (x:xs)
  | x == 0 || x `notElem` xs = isValidSubsection xs
  | otherwise = False

isValidSudoku :: [[Int]] -> Bool
isValidSudoku xss = and [x >= 0 && x <= length xss | x <- concat xss] && 
  and [isValidSubsection (selectSquare xss n) && isValidSubsection (selectRow xss n) && isValidSubsection (selectColumn xss n) | n <- [0..length xss - 1]] 

-- HA 3.1c)
setCell :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
setCell xss (j, k) x = undefined 

-- HA 3.1d)
{-WETT-}
solveSudoku :: [[Int]] -> [[Int]]
solveSudoku xss = undefined
{-TTEW-}

hardSudoku :: [[Int]]
hardSudoku = [[8,0,0,0,0,0,0,0,0],
              [0,0,3,6,0,0,0,0,0],
              [0,7,0,0,9,0,2,0,0],
              [0,5,0,0,0,7,0,0,0],
              [0,0,0,0,4,5,7,0,0],
              [0,0,0,1,0,0,0,3,0],
              [0,0,1,0,0,0,0,6,8],
              [0,0,8,5,0,0,0,1,0],
              [0,9,0,0,0,0,4,0,0]]

-- Utility method to show a sudoku
-- show sudoku with
-- >>> putStr (showSudoku sudoku)
showSudoku :: [[Int]] -> String
showSudoku xss = unlines $ intercalate [showDivider] $ chunksOf squareSize $ map showRow xss
  where
    size = length xss
    squareSize = intRoot size 
    numberSize = size `div` 10 + 1

    showRowSection xs = unwords $ map (printf ("%0" ++ show numberSize ++ "d")) xs
    showRow xs = intercalate "|" $ map showRowSection $ chunksOf squareSize xs
    showDivider = intercalate "+" $ replicate squareSize $ replicate ((numberSize + 1) * squareSize - 1) '-'

    chunksOf :: Int -> [e] -> [[e]]
    chunksOf i [] = []
    chunksOf i ls = take i ls : chunksOf i (drop i ls)
