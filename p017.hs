p017 :: Int -> Int
p017 n =
    sum . -- the sum of
    map length $ --the lengths of
    map (filter (/= ' ')) $ --the space-stripped strings of
    map (filter (/= '-')) $ --the hyphen-stripped strings of
    map numberToText -- the text-conversion of
    [1..n] -- the numbers from 1 to n


ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen",
    "eighteen", "nineteen"]

tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

numberToText :: Int -> String
numberToText 1000 = "one thousand"
numberToText n = concat[hundredsComponent n,subHundredComponent n]

hundredsComponent :: Int -> String
hundredsComponent n
    | (n > 99) = concat[ones !! (n `div` 100), " hundred"]
    | otherwise = ""

subHundredComponent :: Int -> String
subHundredComponent n
    | (n > 99 && (n `mod` 100 /= 0)) = concat [" and ", tensComponent n, onesComponent n]
    | (n > 19) = concat[tensComponent n, onesComponent n]
    | otherwise = ones !! n

tensComponent :: Int -> String
tensComponent n
    | (n `div` 10 >= 2) = tens !! (n `mod` 100 `div` 10)
    | otherwise = ""

onesComponent :: Int -> String
onesComponent n
    | (n `mod` 100 < 20) = ones !! (n `mod` 20)
    | otherwise = concat[prefix, ones !! (n `mod` 10)]
    where prefix = if (n `mod` 100 > 19 && (n `mod` 10 /= 0)) then "-" else ""

result = p017 1000
main = print result
