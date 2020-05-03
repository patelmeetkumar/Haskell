primes (x:xs) = x : primes (filter ((/=0) . (`mod` x)) xs)

nthPrime n = primes [2..] !! (n-1) 

-- to calculate 10001st prime call "nthPrime 10001" in ghci 



