-- Project Euler Problem # 14
-- https://projecteuler.net/problem=14

import Data.Word
import Data.Array

-- use memoization technique to speed up processing time
-- by storing results of expensive function calls
memoizationStorage :: Array Word Word
memoizationStorage = listArray (1, maximumSize) $ map collatz [1..maximumSize]
    -- set size to million to be safe
    where maximumSize = 1000000

collatz :: Word -> Word
collatz 0 = 0
collatz 1 = 1
-- check if the computation is already made for 
collatz number | inRange (bounds memoizationStorage) next = 1 + memoizationStorage ! next
          | otherwise = 1 + collatz next
          where next = case number of
                           1 -> 1
                                  -- n → n/2 (n is even)
                           number | even number -> number `div` 2
                                  -- n → 3n + 1 (n is odd)
                                  | otherwise -> 3 * number + 1

-- 1 to million input each and find the longest chain from all of them
main = print $ snd $ maximum $ map (\n -> (collatz n, n)) [1..1000000]
