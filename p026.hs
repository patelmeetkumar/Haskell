-- Project Euler Problem # 26
-- https://projecteuler.net/problem=26

import Data.List (maximumBy)
import Data.Function (on)

-- get integer as parameter and return integer
recurringCycleLength :: Integer -> Integer
-- use cycleLength for every iteration from 3 to 1000 for denominator
-- use of basic mathematical reasoning, answer lies in odd number
recurringCycleLength number | even number = 0
                            | number `rem` 5 == 0 = 0
                            | otherwise = head [p | p <- [1..], (10^p - 1) `rem` number == 0]

main :: IO ()
-- maximumBy returns the greatest element of list by using compare
-- numerator is set to 1, while denominator goes from 3 to 1000
main = print $ maximumBy (compare `on` recurringCycleLength) [1,3..1000]
