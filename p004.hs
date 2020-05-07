-- Project Euler Problem # 4
-- https://projecteuler.net/problem=4

-- Function to check if a number is palindrome
isItPalindrome ::  Integer -> Bool
isItPalindrome number = show number == reverse (show number)

-- Applied a brute force approach
main ::  IO ()
-- take maximum of X times Y where the product is a palindrome
main = print $ maximum [productOfXY 
  | x <- [100..999], y <- [x..999], 
  let productOfXY = x * y, isItPalindrome productOfXY]
