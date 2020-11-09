module Practice
    ( mainFunc
    ) where

import Data.Char

-- Count number of letters in a string
countString :: String -> Int
countString [] = 0
countString [c] | isLetter c = 1 | otherwise = 0
countString (c0:c1:s') = counter + countString (c1:s')
    where counter = if isLetter c0 && not (isLetter c1) then 1 else 0


-- Reverse a list 
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

mainFunc :: IO ()
mainFunc = putStrLn $ show $ rev "hello"