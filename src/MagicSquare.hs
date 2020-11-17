module MagicSquare
    ( outFunc
    ) where

import Data.List

sideLength :: Int
sideLength = 3

-- Solution 1
sumIs15 :: [Int] -> Bool
sumIs15 xs = sum xs == 15

groupByRow :: [Int] -> [[Int]]
groupByRow xs = [snd (unzip (filter (\x -> fst x `div` sideLength == n) (zip [0..(sideLength^2-1)] xs))) | n <- [0..sideLength - 1]]
-- groupByRow xs = loop 0
--     where   loop n  | n > sideLength - 1 = []
--                     | otherwise = (row n):(loop (n + 1))
--             row n = snd (unzip (filter (\x -> (fst x) `div` sideLength == n) (zip [0..(sideLength^2-1)] xs)))

groupByCol :: [Int] -> [[Int]]
groupByCol xs = [snd (unzip (filter (\x -> fst x `mod` sideLength == n) (zip [0..(sideLength^2-1)] xs))) | n <- [0..sideLength - 1]]
-- groupByCol xs = loop 0
--     where   loop n  | n > sideLength - 1 = []
--                     | otherwise = (col n):(loop (n + 1))
--             col n = snd (unzip (filter (\x -> (fst x) `mod` sideLength == n) (zip [0..(sideLength^2-1)] xs)))


groupByMainDiag :: [Int] -> [[Int]]
-- groupByMainDiag xs = [diag, antidiag]
--     where   diag = findDiag 0
--             antidiag = findAntiAntidiag 0
--             findDiag n  | n > sideLength - 1 = []
--                         | otherwise = (xs!!(n * sideLength + n)):(findDiag (n + 1))
--             findAntiAntidiag n  | n > sideLength - 1 = []
--                                 | otherwise = ((xs!!((n+1) *sideLength - 1)):(findAntiAntidiag (n + 1)))
groupByMainDiag xs = [diag, antidiag]
    where   (diag, antidiag) = findDiag 0
            findDiag n  | n > sideLength - 1 = ([], [])
                        | otherwise = ((xs!!(n * sideLength + n)):fst (findDiag (n + 1)), ((xs!!((n + 1) * 2)):snd (findDiag (n + 1))))

ok :: [Int] -> Bool
ok xs = all (\x -> sumIs15 x) (groupAll xs)
    where   groupAll xs = groupByCol xs ++ groupByRow xs ++ groupByMainDiag xs

magicSquare :: [[Int]]
magicSquare = [xs | xs <- permutations[1..sideLength^2], ok xs]


-- Solution 2
matrix :: [Int] -> [[Int]]
matrix [] = []
matrix vec = take sideLength vec : matrix (drop sideLength vec)

allOk :: [Int] -> Bool
allOk xs =  and $ map (==15) $ map (\x -> sum x) (groupByRow ++ groupByCol ++ [diag, antidiag])
    where   groupByRow = xs'
            groupByCol = transpose xs'
            (diag, antidiag) = findDiag 0
            findDiag n  | n > sideLength - 1 = ([], [])
                        | otherwise = (xs'!!n!!n:fst (findDiag (n + 1)), xs'!!n!!(sideLength - 1 - n): snd (findDiag (n + 1)))
            xs' = matrix xs

magicSquare2 :: [[Int]]
magicSquare2 = [xs | xs <- permutations[1..sideLength^2], allOk xs]

-- Solution 2'
magicSquare2' :: [[Int]]
magicSquare2' = filter (test . matrix) candidates
    where   test xs = and $ map (==15) $ map (\x -> sum x) (xs ++ transpose xs ++ [diag 0 xs, antiDiag 0 xs])
            diag n xs   | n > sideLength - 1 = []
                        | otherwise = xs!!n!!n: diag (n + 1) xs
            antiDiag n xs   | n > sideLength - 1 = []
                            | otherwise = xs!!n!!(sideLength - 1 - n): antiDiag (n + 1) xs
            matrix [] = []
            matrix vec = take sideLength vec : matrix (drop sideLength vec)
            candidates = permutations [1..sideLength^2]
            

outFunc :: IO ()
outFunc = putStrLn $ show $ length magicSquare2'