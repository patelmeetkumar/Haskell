-- Find count of distinct right-angled triangles with one point at (0,0) and the remaining points on ([0..50], [0..50])

-- Rather than brute-forcing the solution, the RATs given by O, P1, P2 with P1.x > P2.x and P1.y > P2.y may be thought of as the sum of:
-- 1. The set of RATs with their RAs at P1
-- 2. The set of RATs with their RAs at P2 (which is a mirror of the first set about the line x=y
-- 3. The set of RATs with RA at either x=0 or y=0

-- The number of triangles in set one for a given P1 may be found by extending edge P1P2 down/right by the required ratio of units until P2 exceeds the lowermost or rightmost edge.
-- The number of triangles in set two is equal to that in set 1.
-- The number of triangles in each of the subsets of set three is equal to the number of possible permutations ([1..gridSize], [1..gridSize]) = gridSize^2 => 3*(gridsize^2) total

import Data.Ratio

data Point = Point Int Int deriving (Eq, Show)

solution = solution' 50

solution' :: Int -> Int
solution' n =
    (3*(n^2)) + -- the set of RATs with RA at either x=0 or y=0, plus 
    2 * -- the double of 
    (sum $ -- the sum of
    [(trianglesHaving p1 n) | x <- [1..n], y <- [1..n], let p1 = Point x y]) --the list of the number of RATs having a RA at P1, P1 not on an axis.

trianglesHaving :: Point -> Int -> Int
trianglesHaving (Point x y) gridSize =
    min (y `div` (numerator dx)) ((gridSize - (x)) `div` denominator dx)
    where dx = x % y
