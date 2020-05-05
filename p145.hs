-- P145: How many reversible numbers are there below one-billion?
-- N.B. n 'reversible' => n + reverse(n) has only odd digits.
--      any leading zeroes in reverse(n) => n irreversible

import Data.Char

solution = solution' $ 10^9

solution' :: Int -> Int
solution' n = length $ -- the length of
    filter isReversible $ -- the reversible elements of
    optimise -- the optimised subset of
    [1..n] -- the list of inputs

optimise :: [Int] -> [Int]
optimise l = 
    filter (\x -> x <= 10^8) $ -- strip the most-onerous set of inputs with odd digit count (auto-fail)
    l

isReversible :: Int -> Bool
isReversible n
    | (n < 1) = False -- fail non-positive numbers
    | (n `mod` 10 == 0) = False -- fail numbers resulting in leading zeroes
    | otherwise = -- otherwise return
        containsOnlyOddDigits $  -- the odd-digit-ness of
            n + reverseInt n  -- the sum of n and reverse(n)

containsOnlyOddDigits :: Int -> Bool
containsOnlyOddDigits n =
    length [x | x <- (show n), even $ digitToInt x] == 0

reverseInt :: Int -> Int
reverseInt n = 
    read . -- the Int read of
    reverse . --the string-reversal of
    show $ n -- the string representation of n
