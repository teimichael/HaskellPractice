module FPPractice
    ( _gcd
    , range
    , insert
    , isort
    , sumList
    , oddplus1
    , merge
    , split
    , msort
    , nub
    , myUnzip
    , power
    , outFunc
    ) where

_gcd :: Int -> Int -> Int
_gcd x y
    | y == 0 = x
    | y > x = _gcd y x
    | otherwise = _gcd (x - y) y

range :: Int -> Int -> [Int]
range m n
    | m > (n + 1) = [m, m - 1..n]
    | n > (m + 1) = [m..n]
    | otherwise   = []

range' :: Int -> Int -> [Int]
range' m n
    | m > n = []
    | otherwise = m:range' (m + 1) n

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) =
    let smaller = [a | a <- y:ys, a <= x]
        bigger  = [a | a <- y:ys, a > x] 
    in  smaller ++ [x] ++ bigger

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

sumList :: (Num a) => [a] -> a
sumList xs = foldr (\acc x -> acc + x) 0 xs

oddplus1 :: [Int] -> [Int]
oddplus1 xs = [x + 1 | x <- xs, x `mod` 2 == 1]

oddplus1' :: [Int] -> [Int]
oddplus1' xs = map (+1) (filter (\x -> mod x 2 == 1) xs)


merge :: [Int] -> [Int] -> [Int]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys)
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)

split :: [Int] -> ([Int], [Int])
split [] = ([],[])
split [x] = ([x], [])
-- split (x:y:xs) = (firstHalf, secondHalf)
--     where firstHalf = x: fst (split xs)
--           secondHalf = y: snd (split xs)
split (x:y:zs) = (x:xs, y:ys)
    where (xs, ys) = split zs

-- Merge sort
msort :: [Int] -> [Int]
msort [] = []
msort [a] = [a]
msort xs = merge (msort ys) (msort zs)
    where (ys, zs) = split xs


-- Remove duplicate elements from a list
nub :: (Eq a) => [a] -> [a]
nub (x:xs) = x : nub (filter (/= x) xs)
nub [] = []


myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((a, b):zs) = (a:xs, b:ys)
    where (xs, ys) = myUnzip zs

-- myUnzip [] = ([], []) 
-- myUnzip xs = (map fst xs, map snd xs)

-- unzip' :: [(a,b)] -> ([a],[b])
-- unzip' = fold (\x acc -> (fst x:fst acc, snd x:snd acc)) ([],[])

power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = map (x:) (power xs) ++ power xs


outFunc :: IO ()
outFunc = putStrLn $show $ power [1, 2]