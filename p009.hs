-- Project Euler Problem # 9
-- https://projecteuler.net/problem=9

main ::  IO ()
-- find product of abc where a+b+c = 1000 and a^2 + b^2 = c^2.
main = print $ head [a*b*c | 
  -- values and range of 500 determined by the laws from geometry
  a <- [1..500], b <- [a..500], let c = 1000 - a - b,
  -- requirements per the instructions
  (a + b + c) == 1000, (a * a) + (b * b) == (c * c)]

  
