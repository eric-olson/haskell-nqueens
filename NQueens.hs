-- N-Queens homework
-- Eric Olson

import Data.Ix
import Data.List
import Data.List.Split

-- generates all permutations of 2nd coordinates of board positions (assuming 1st coordinates are [1..n])
genPermutations :: Int -> [[Int]]
genPermutations 0 = [[]]
genPermutations n = permutations [1..n]

-- genCoords takes a list of permutations and converts them to fully defined coordinates
genCoords :: [[Int]] -> [[(Int, Int)]]
genCoords xs = map (zip [1..]) xs

-- genBoard returns a list of board positions for a given size
genBoard :: Int -> [(Int, Int)]
genBoard n = range ((1,1),(n,n))

-- checkDiagonal returns true if two coordinates are diagonal to each other
checkDiagonal :: (Int, Int) -> (Int, Int) -> Bool
checkDiagonal (x1, y1) (x2, y2) = abs(x1 - x2) == abs(y1 - y2)

-- containsDiag checks a solution for diagonally aligned queens
containsDiag :: [(Int, Int)] -> Bool
containsDiag [] = False
--containsDiag (x:y:[]) = checkDiagonal x y
containsDiag (x:xs) = (True `elem` map (checkDiagonal x) xs) || containsDiag xs

-- removeDiags removes any solutions containing diagonally aligned queens
removeDiags :: [[(Int, Int)]] -> [[(Int,Int)]]
removeDiags xs = filter (not.containsDiag) xs

-- showCell outputs the correct character to display for a cell
showCell :: [(Int, Int)] -> (Int, Int) -> String
showCell xs x
  | x `elem` xs = "Q "
  | otherwise = "X "

-- showBoard prints a solution given solution locations and size
-- mapM_ putStrLn $ map unwords $ chunksOf 4 $ map (showCell solution) board
showBoard :: Int -> [(Int, Int)] -> IO ()
showBoard n xs = do
  putStrLn ""
  mapM_ putStrLn $ map unwords $ chunksOf n $ map (showCell xs) $ genBoard n
  putStrLn ""

-- getSolns generates all solutions for a given size
getSolns :: Int -> [[(Int, Int)]]
getSolns n = removeDiags $ genCoords $ genPermutations n

-- nQueens displays all solutions for a given size
showNQueens :: Int -> IO ()
showNQueens n
  | length solns > 0 = mapM_ (showBoard n) $ solns
  | otherwise = putStrLn "No solutions"
  where solns = getSolns n

-- nQueensNumSoln outputs the number of solutions for an N-Queens problem
showNQueensNumSoln :: Int -> IO ()
showNQueensNumSoln n = print $ length $ getSolns n

-- prompt (EC)
nQueens :: IO ()
nQueens = do
  putStr "Enter size of board: "
  size <- getLine
  showNQueens (read size :: Int)

nQueensNumSoln :: IO ()
nQueensNumSoln = do
  putStr "Enter size of board: "
  size <- getLine
  showNQueensNumSoln (read size :: Int)
