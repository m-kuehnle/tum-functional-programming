module Exercise05 where
import Data.List

-- May or may not be useful: computes the logarithm base 2 (rounded down) of the given number.
-- You don't have to move this into the WETT tags if you want to use it.
log2 :: (Integral a, Num b) => a -> b
log2 = let go acc n = if n <= 1 then acc else go (acc + 1) (n `div` 2) in go 0

{-WETT-}
decompose :: [Integer] -> [Integer]
decompose [] = []
decompose [0] = []
decompose [x] = dec1 (2^log2 x) [x]
decompose ds = if 0 `elem` ds || res == [0] then [] else res
    where tups = decc (2^log2 (minimum ds)) ds
          res = if length tups == 1 then [snd (head tups)] else let l = last tups in decc2 len (init i) (last i) [snd l] ((fst l ^ len) * snd l)
            where len = length ds
                  i = init tups  

dec1 :: Integer -> [Integer] -> [Integer]
dec1 0 _ = []
dec1 start [l] = dec1 (start `div` 2) [l `mod` start] ++ [l `div` start]

-- (len, count)
decc :: Integer -> [Integer] -> [(Integer, Integer)]
decc 1 ds = [(1, product ds)]
decc start ds = decc (start `div` 2) ds ++ [(start, product [d `div` start | d <- ds])]
    
decc2 :: Int -> [(Integer, Integer)] -> (Integer, Integer) -> [Integer] -> Integer -> [Integer]
decc2 d rest e fix akku = if fst e == 1 then eAnzNeu : fix else decc2 d (init rest) (last rest) (eAnzNeu : fix) (akku + ((fst e ^ d) * eAnzNeu))
    where eAnzNeu = snd e - akku `div` (fst e ^ d)
{-TTEW-}
