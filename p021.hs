-- basic definition of amicable numbers 
amicable :: Int -> Bool
amicable n = let m = sumOfDivisors n in (m /= n) && (sumOfDivisors m) == n

-- used to calculate the sum of a number's divisors 
sumOfDivisors :: Int -> Int
sumOfDivisors n = sum [k | k <- [1..n-1], (mod n k) == 0]

ans = sum [n | n <- [1..10^4], amicable n]
main = putStrLn (show ans)
