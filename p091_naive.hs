-- Find count of distinct right-angled triangles with one point at (0,0) and the remaining points on ([0..50], [0..50])

-- Naive solution which generates all permutations of points and tests each one
-- It is feasibly fast, but exponentially slower than it needs to be
-- It is also not completely accurate, because the floating-point comparison for equality fails for small deviations from 90deg

data Point = Point Int Int deriving (Eq, Show)

data Triangle = Triangle Point Point Point deriving (Show)

solution = solution' 50

solution' :: Int -> Int
solution' n = 
    let 
        candidates = [
            (Triangle o p1 p2) | 
            x1 <- [0..n],
            y1 <- [0..n],
            x2 <- [0..n],
            y2 <- [0..n],
            let o = (Point 0 0),
            let p1 = (Point x1 y1),
            let p2 = (Point x2 y2),
            p1 /= o,
            p2 /= o,
            p1 /= p2
            ]
    in
    -- because of duplication for triangles (0, p1, p2) and (0, p2, p1), it is necessary to half the result
        (\n -> n `div` 2) $ -- half of
        length $ -- the number of
        filter (isRightAngleTriangle) -- triangles which are right-angled from
        candidates -- the list of possible distinct triangles

isRightAngleTriangle :: Triangle -> Bool
isRightAngleTriangle (Triangle a b c) =
    abs (angle a b c - 90) < 0.00001 ||
    abs (angle b c a - 90) < 0.00001 ||
    abs (angle c a b - 90) < 0.00001

angle :: Point -> Point -> Point -> Float 
 -- returns the absolute degree angle subtended by an arc from A to C, centered at B
angle a b c =
    theta_degrees
    where
        ab= distance a b
        bc= distance b c
        ac= distance a c
        theta_radians = acos((bc^2 + ab^2 - ac^2)/(2*bc*ab))
        theta_degrees = theta_radians * 180 / pi

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt $ fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2


candidates = [
            (Triangle o p1 p2) | 
            x1 <- [0..2],
            y1 <- [0..2],
            x2 <- [0..2],
            y2 <- [0..2],
            let o = (Point 0 0),
            let p1 = (Point x1 y1),
            let p2 = (Point x2 y2),
            p1 /= o,
            p2 /= o,
            p1 /= p2
            ]

rats = filter isRightAngleTriangle candidates
