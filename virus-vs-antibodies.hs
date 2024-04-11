module Exercise08 where

import Data.Bits
import Data.List
import System.Random (Random, mkStdGen, randomIO, randoms)

-- Player is either 1 or -1
type Player = Int

-- A field is just an Int value where the absolute gives the number of pieces on the field
-- and the sign corresponds to the player
-- e.g. -3 would mean there are three blobs in this field of player -1
type Field = Int

type Row = [Field]

type Column = [Field]

-- boards are rectangles represented as a list of rows
type Board = [Row]

-- A position on the board is represented as (row, column)
-- (0,0) is the top left corner, coordinate values increase towards the bottom right
type Pos = (Int, Int)

-- A size represented as (height,width)
type Size = (Int, Int)

-- A strategy takes the player who's move it is, optionally takes a list of double values
-- to allow for probabilistic strategies, takes the current board and gives back the position
-- of the move the player should do
type Strategy = [Double] -> Player -> Board -> Pos

-- A stateful strategy can additionally pass some object between invocations
type StatefulStrategyFunc a = a -> [Double] -> Player -> Board -> (Pos, a)

-- first value is the state object to pass to the first invocation of each game
type StatefulStrategy a = (a, StatefulStrategyFunc a)

defaultSize :: (Int, Int)
defaultSize = (9, 6)

-- Some useful helper functions
row :: Board -> Int -> Row
row = (!!)

column :: Board -> Int -> Column
column = row . transpose

width :: Board -> Int
width (x : _) = length x
width _ = 0

height :: Board -> Int
height = length

size :: Board -> Size
size b = (height b, width b)

getCell :: Pos -> Board -> Field
getCell (y, x) b = b !! y !! x

-- pretty print a single cell
showCell :: Field -> String
showCell c = "- +" !! succ (signum c) : show (abs c)

-- pretty print the given board
showBoard :: Board -> String
showBoard = unlines . map (unwords . map showCell)

-- print a board to the console
printBoard :: Board -> IO ()
printBoard = putStr . showBoard

-- check if a position is on a board of the given size
isValidPos :: Size -> Pos -> Bool
isValidPos (r, c) (y, x) = y >= 0 && y < r && x >= 0 && x < c

{- x.1 -}

-- Check if the given player can put an orb on the given position
canPlaceOrb :: Player -> Pos -> Board -> Bool
canPlaceOrb 1 (y, x) b = isValidPos (size b) (y, x) && getCell (y, x) b >= 0
canPlaceOrb (-1) (y, x) b = isValidPos (size b) (y, x) && getCell (y, x) b <= 0

-- Check if the given player has won the game,
-- you can assume that the opponent has made at least one move before
hasWon :: Player -> Board -> Bool
hasWon 1 b = minimum (concat b) >= 0
hasWon (-1) b = maximum (concat b) <= 0

-- the list of neighbors of a cell.
neighbors :: Size -> Pos -> [Pos]
neighbors (r, c) (y, x) = filter (isValidPos (r, c)) [(y + 1, x), (y -1, x), (y, x + 1), (y, x -1)]

-- update a single position on the board
-- f: function that modifies the number of orbs in the cell
-- p: player to whom the updated cell should belong
updatePos :: (Int -> Int) -> Player -> Pos -> Board -> Board
updatePos f p (y, x) b = fst partsRow ++ [v ++ (neu : h)] ++ tail (snd partsRow)
  where
    partsRow = splitAt y b
    liste = splitAt x $ head (snd partsRow)
    v = fst liste
    neu = f (abs $ head (snd liste)) * p
    h = tail (snd liste)

{- x.2 -}

-- place an orb for the given player in the given cell
putOrb :: Player -> Pos -> Board -> Board
putOrb p (y, x) b
  | hasWon p b && not (all (== 0) (concat b)) = b
  | abs (getCell (y, x) b) + 1 < lenN = updatePos (+ 1) p (y, x) b
  | otherwise = if lenN == 2 then two else if lenN == 3 then three else four
  where
    neigh = neighbors (size b) (y, x)
    lenN = length neigh
    two = putOrb p (neigh !! 1) $ putOrb p (head neigh) $ updatePos (const 0) p (y, x) b
    three = putOrb p (neigh !! 2) two
    four = putOrb p (neigh !! 3) three

{- x.3 -}

{-WETT-}
-- Your strategy.
strategy :: Strategy
strategy _ p b = pNext p b (0, 0) ((0, 0), -10000000)

-- next position to set for player
pNext :: Player -> Board -> Pos -> (Pos, Int) -> Pos
pNext p b curPos (maxPos, maxScore)
  | curPos == (-1, -1) = maxPos
  | canPlaceOrb p curPos b && curScore > maxScore = pNext p b nxtPos (curPos, curScore)
  | otherwise = pNext p b nxtPos (maxPos, maxScore)
  where
    nxtPos = nextPos curPos b
    myMoveB = putOrb p curPos b
    oppMoveB = let oppPos = oppNext (p * (-1)) myMoveB (0, 0) ((0, 0), -10000000) in putOrb (p * (-1)) oppPos myMoveB
    curScore = 2 * eval p oppMoveB + eval p myMoveB

-- assumed move for opponent
oppNext :: Player -> Board -> Pos -> (Pos, Int) -> Pos
oppNext p b curPos (maxPos, maxScore)
  | curPos == (-1, -1) = maxPos
  | canPlaceOrb p curPos b && curScore > maxScore = oppNext p b nPos (curPos, curScore)
  | otherwise = oppNext p b nPos (maxPos, maxScore)
  where
    nPos = nextPos curPos b
    curScore = eval p $ putOrb p curPos b

-- evaluate given board for given player and return score
eval :: Player -> Board -> Int
eval p b
  | hasWon (p * (-1)) b = -1000000
  | hasWon p b = 1000000
  | otherwise = countBodies p b - cInDanger p b (0, 0) 0 + cInDanger (p * (-1)) b (0, 0) 0

-- returns the number of orbs of the given player, that are in danger of being overflown by opponent-neighbour-cells
cInDanger :: Player -> Board -> Pos -> Int -> Int
cInDanger p b curPos acc
  | curPos == (-1, -1) = acc
  | signum c == p = cInDanger p b nextP (acc + neighOv)
  | otherwise = cInDanger p b nextP acc
  where
    c = getCell curPos b
    nextP = nextPos curPos b
    curNeigh = neighbors (size b) curPos
    neighOv = if not (any (\n -> orbsUntilOverflow n b < orbsUntilOverflow curPos b && signum (getCell n b) == p * (- 1)) curNeigh) then - abs c else abs c

-- number of orbs in cell required for overflow
orbsUntilOverflow :: Pos -> Board -> Int
orbsUntilOverflow pos b = length (neighbors (size b) pos) - abs (getCell pos b)

countBodies :: Player -> Board -> Int
countBodies p b = foldr (\x acc -> if signum x == p then abs x + acc else acc) 0 (concat b)

-- returns position of next cell on given board
nextPos :: Pos -> Board -> Pos
nextPos (y, x) b
  | y == height b - 1 && x == width b - 1 = (-1, -1) -- end of row and col
  | x == width b - 1 = (y + 1, 0) -- end of row
  | otherwise = (y, x + 1)

-- adds state to a strategy that doesn't use it
wrapStrategy :: Strategy -> StatefulStrategy Int
wrapStrategy strat = (0, \s r p b -> (strat r p b, succ s))

-- the actual strategy submissions
-- if you want to use state modify this instead of strategy
-- additionally you may change the Int in this type declaration to any type that is usefully for your strategy
strategyState :: StatefulStrategy Int
strategyState = wrapStrategy strategy

{-TTEW-}

-- Simulate a game between two strategies on a board of the given size and
-- returns the state of the board before each move together with the player that won the game
play :: [Int] -> Size -> StatefulStrategy a -> StatefulStrategy b -> [(Board, Pos)]
play rss (r, c) (isa, sa) (isb, sb) = go rss isa sa isb sb 1 0 (replicate r (replicate c 0))
  where
    -- type signature is necessary, inferred type is wrong!
    go :: [Int] -> a -> StatefulStrategyFunc a -> b -> StatefulStrategyFunc b -> Player -> Int -> Board -> [(Board, Pos)]
    go (rs : rss) stc sc stn sn p n b
      | won = []
      | valid = (b, m) : go rss stn sn st' sc (- p) (succ n) (putOrb p m b)
      | otherwise = []
      where
        won = n > 1 && hasWon (- p) b
        (m, st') = sc stc (mkRandoms rs) p b
        valid = isValidPos (size b) m && canPlaceOrb p m b

-- Play a game and print it to the console
playAndPrint :: Size -> StatefulStrategy a -> StatefulStrategy b -> IO ()
playAndPrint size sa sb = do
  seed <- randomIO
  -- let seed = 42
  let moves = play (mkRandoms seed) size sa sb
  putStr $
    unlines (zipWith showState moves $ cycle ['+', '-']) ++ "\n"
      ++ (case length moves `mod` 2 of 1 -> "Winner: +"; 0 -> "Winner: -")
      ++ "\n"
      ++ "View at https://vmnipkow16.in.tum.de/christmas2020/embed.html#i"
      ++ base64 (1 : t size ++ concatMap (t . snd) moves)
      ++ "\n"
  where
    showState (b, pos) p = showBoard b ++ p : " places at " ++ show pos ++ "\n"
    t (a, b) = [a, b]

mkRandoms :: Random a => Int -> [a]
mkRandoms = randoms . mkStdGen

base64 :: [Int] -> String
base64 xs = case xs of
  [] -> ""
  [a] -> f1 a : f2 a 0 : "=="
  [a, b] -> f1 a : f2 a b : f3 b 0 : "="
  a : b : c : d -> f1 a : f2 a b : f3 b c : f4 c : base64 d
  where
    alphabet = (!!) "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    f1 a = alphabet $ shiftR a 2
    f2 a b = alphabet $ shiftL (a .&. 3) 4 .|. shiftR b 4
    f3 b c = alphabet $ shiftL (b .&. 15) 2 .|. shiftR c 6
    f4 c = alphabet $ c .&. 63
