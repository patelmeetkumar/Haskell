--for transpose--
import Data.List

--products from a row--
subLists :: [Int] -> [Int]
subLists xs 
  | xs == [] = []
  | otherwise = [product . take 4 $ xs] ++ subLists (tail xs)

--feeds rows to the subLists function--
horiProduct :: [[Int]] -> [Int]
horiProduct grid 
  | grid == [] = []
  | otherwise = (subLists . head $ grid) ++ horiProduct (tail grid)
  
--creates a diagonal from a grid and starting position in the first row--
getDiag :: [[Int]] -> Int -> [Int]
getDiag grid pos
  | grid == [] = []
  | drop pos (head grid) == [] = []
  | otherwise = [head . drop pos . head $ grid] ++ getDiag (tail grid) (succ pos)
  
--gets all diagonals from a grid by a row--
diagRow :: [[Int]] -> Int -> [Int]
diagRow grid pos
  | drop pos (head grid) == [] = []
  | otherwise = (subLists (getDiag grid pos)) ++ diagRow grid (succ pos)

--feeds rows to diagRow--
diagProduct :: [[Int]] -> [Int]
diagProduct grid
  | grid == [] = []
  | otherwise = (diagRow grid 0) ++ diagProduct (tail grid)

--finds the maximum by calling horizontal, vertical, and diagonal products--
largestProduct :: [[Int]] -> Int
largestProduct grid =
  maximum $ (horiProduct grid) ++ (horiProduct . transpose $ grid) ++ (diagProduct grid) ++ (diagProduct . map reverse $ grid)

--reads the file in and calls largestProduct--
main = do
  content <- readFile "p011_grid.txt"
  let wordGrid = map words $ lines content
  let grid = map ( map ( read :: String -> Int )) $ wordGrid
  print . largestProduct $ grid