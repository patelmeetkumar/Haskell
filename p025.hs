-- infinite list of fib numbers
fib :: [Integer]
fib = 0 : 1 : (zipWith (+) fib (tail fib))

-- keep "taking" from infinite list until we reach a num with 1000 digits
-- print length of list 
digits = 1000
ans = length (takeWhile (< 10^(digits-1)) fib)
main = putStrLn (show ans) 
