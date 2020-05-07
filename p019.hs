-- Project Euler Problem # 19
-- https://projecteuler.net/problem=19

-- the number of days in each month respectively - January to December
allMonthLengths = [31,28,31,30,31,30,31,31,30,31,30,31]
-- Years except century are leapYear if evenly divisible by 4
-- Century years are leapYear if evenly divisible by 400
isLeap year = year `mod` 4 == 0 && year `mod` 100 /= 0 || year `mod` 400 == 0
-- format for parameter and it's meaning
nextMonthDay (year, 11, day) = (year + 1, 0, (day + 31) `mod` 7)
-- if leap year, adjust the month of February
nextMonthDay (year, 1, day) | isLeap year = (year, 2, (day + 29) `mod` 7)
                            | otherwise   = (year, 2, (day + 28) `mod` 7)
nextMonthDay (year, month, day) = (year, month + 1, (day + allMonthLengths !! month) `mod` 7)

-- parameter represent YYYY, MM, DD, start with January 1, 1901
iterateTime = iterate nextMonthDay (1900,0,0)
-- all we care about is Sunday which is 6.
isSunday (_,_,6) = True
isSunday _       = False
-- comparision to compare program year value with parameter inputYear
compareYears inputYear (year,_,_) = year < inputYear
-- count the number of Sundays between years from 1901 to 2000
countSundays = length (filter isSunday (takeWhile (compareYears 2001) (dropWhile (compareYears 1901) iterateTime)))

-- main program to find solution
main :: IO ()
main = print countSundays
