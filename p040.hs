-- decimal fraction
decimal = concat $ map digits [1..] 

-- function to find all the digits we need 
digits 0 = [0]
digits n = reverse (digits' n) where
 digits' 0 = []
 digits' n = (mod n 10) : (digits' (div n 10))

-- multiply digits 
ans = product [decimal !! (10^i - 1) | i <- [0..6]]
main = putStrLn (show ans) 