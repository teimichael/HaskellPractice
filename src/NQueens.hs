module NQueens
    ( outFunc
    ) where

-- Solution 1
import Data.List
safe :: (Int, Int) -> (Int, Int) -> Bool
safe (x, y) (x', y') = (x /= x') && (y /= y') && ((abs (x - x')) /= (abs (y - y')))

ok :: [Int] -> Bool
ok xs = test (zip [0..(length xs) - 1] xs)
    where test [] = True
          test (q:qs) = (all (\x -> safe q x) qs) && test qs


nqueen :: Int -> [[Int]]
nqueen n = [xs | xs <- permutations [0..n-1], ok xs]

-- Solution 1'
nqueen' :: Int -> [[Int]]
nqueen' n = [xs | xs <- permutations [0..n-1], ok xs]
    where ok candidates = test (zip [0..n-1] candidates)
          test [] = True
          test (p:ps) = (all (\x -> safe p x) ps) && test ps
          safe (x, y) (x', y') = (x /= x') && (y /= y') && ((abs (x - x')) /= (abs (y - y')))
          


-- Solution 2
queens :: Int -> [[Int]]
queens n = filter (\x -> test (zip [1..n] x)) candidates
    where candidates = permutations [1..n]
          unSafe (x1, y1) (x2, y2) = (y1 == y2) || ((abs (x1 - x2)) == (abs (y1 - y2)))
          test [] = True
          test (q:qs) = (not $ (any (\x -> unSafe q x) qs)) && (test qs)

outFunc :: IO ()
outFunc = putStrLn $ show $ length $ nqueen 8