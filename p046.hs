-- Project Euler Problem # 46
-- https://projecteuler.net/problem=46

-- determine if a imput-parameter number is a prime number
isItPrimeNumber :: Integer -> Bool
isItPrimeNumber number | number < 1 = False
                       | otherwise = not $ or [number `rem` temp == 0 | temp <- [2..floor $ sqrt $ fromIntegral number]]
-- determine if input-parameter number equals to sum of a prime and twice a square
fitsGoldbachConjecture :: Integer -> Bool
fitsGoldbachConjecture number = any isItPrimeNumber [number - (2*(squareNumber^2)) | squareNumber <- [0..floor $ sqrt $ fromInteger number]]

answers :: [Integer]
-- the answer could only be a odd number, so we will iterate all odd numbers until finding one
answers = [number | number <- [1,3..], not (isItPrimeNumber number), not (fitsGoldbachConjecture number)]

main :: IO ()
main = print $ head answers
