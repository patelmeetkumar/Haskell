isPrime n = all (not.(`divides` n)) [2..n-1]
 where d `divides` n = n `mod` d == 0

primes = filter isPrime [2..]

primeGenerator n = filter isPrime [2..n]

summing xs = if length xs == 0 then 0 else (head xs) + summing (tail xs)

-- calculate sum of primes under n 
-- "let primeNumbers = primeGenerator n"
-- "summing primeNumbers" 