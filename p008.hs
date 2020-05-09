import Data.Array
import Data.Ix
import Data.Char
import Data.List

-- Project Euler Problem 008 
-- Maximal product of digits from a subsequence of length m from a numerical string of length n

-- Poor non-paradigmatic O(n) attempt

p008_string = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

p008_m= 13 :: Int

p008_bad :: String -> Int -> Int
p008_bad digits digitsToMultiply = p008_bad' initial_head arr digitsToMultiply
    where 
        len = length digits
        max_idx = len - 1
        initial_head = len - digitsToMultiply
        arr = listArray (0, max_idx) [digitToInt x | x <- digits]

p008_bad' :: Int -> Array Int Int -> Int -> Int

p008_bad' i arr digitsToMultiply
    | digitsToMultiply == length arr = --trivial case
        product arr
    | sum arr == 0 = --all-zero case
        0
    | i == length arr - digitsToMultiply = --initial case
        let 
            start_idx = length arr - digitsToMultiply
            end_idx = length arr - 1
        in
            product [arr!j | j <- [start_idx..end_idx], arr!j > 0]
    | i == 0 =  --base case
        let 
            previousMax = p008_bad' 1 arr digitsToMultiply
            multiplier = arr!i
            divisor = arr!(i + digitsToMultiply)
        in max
            previousMax
            ((previousMax `div_if_nonzero` divisor) `product_if_nonzero` multiplier) 
    | otherwise =  --general case
        let 
            previousMax = p008_bad' (i + 1) arr digitsToMultiply
            multiplier = arr!i
            divisor = arr!(i + digitsToMultiply)
        in max 
        previousMax
        ((previousMax `div_if_nonzero` divisor) `product_if_nonzero` multiplier) 
            where 
                previousMax = p008_bad' previousHeadIdx arr digitsToMultiply
                previousHeadIdx = i + 1

product_if_nonzero :: Int -> Int -> Int
product_if_nonzero a b
    | a == 0 = b
    | b == 0 = a
    | otherwise = a * b


div_if_nonzero :: Int -> Int -> Int
div_if_nonzero a b
    | b == 0 = a
    | otherwise = a `div` b


-- Paradigmatic attempt which is O(n+m) but is vastly more expressive and easier to reason about

p008_good :: String -> Int -> Int
p008_good str m = 
    maximum . -- the maximum of
    map (product . take m) $ -- the products of the first m elements of 
    tails $ -- the list of suffixes of
    map digitToInt str -- the list of Int-converted digits in str

result = p008_good p008_string p008_m
main = print result
