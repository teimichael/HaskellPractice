import FPPractice
import Tree
import BST
import NQueens
import MagicSquare

main :: IO ()
main = do
    putStrLn "-- FPPractice --"
    putStrLn $ show $ power [1, 2]
    putStrLn "-- Tree --"
    Tree.outFunc
    putStrLn "-- Binary Search Tree --"
    BST.outFunc
    putStrLn "-- N Queen Problem --"
    NQueens.outFunc
    putStrLn "-- Magic Square Problem --"
    MagicSquare.outFunc
    return ()