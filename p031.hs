-- Given coins of value [1c, 2c, 5c, 10c, 20c, 50c, $1, $2], how many ways to get value $2?

-- This approach leverages Haskell's lazy evaluation feature as an implicit form of memoisation to facilitate dynamic programming.

import Data.List

getCombinations :: [Int] -> Int -> Int

getCombinations coins amount = getCombinations' (reverse $ sort coins) amount

getCombinations' :: [Int] -> Int -> Int
getCombinations' _ 0 = 1
getCombinations' [] _ = 0
getCombinations' coins amount 
    | (amount < 0) = 0
    | otherwise =
        let
            notUseHighest = getCombinations' (tail coins) amount
            useHighest = getCombinations' (coins) (amount - head coins)
        in 
            notUseHighest + useHighest 

result = getCombinations [1,2,5,10,20,50,100,200] 200 
main = print result
