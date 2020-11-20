module KillerSudoku.Main
    ( consoleFunc,
      testFunc
    ) where

import System.Environment ( getArgs )
import Data.List ( transpose )

-- Define expression structure
data Expr =
      Val Int
    | Var String
    | Plus [Expr]
    | Sub [Expr]
    | Mul [Expr]

-- Define formula structure
data Formula =
      And [Formula]
    | Or [Formula]
    | Eq Expr Expr
    | Gt Expr Expr
    | Lt Expr Expr
    | Geq Expr Expr
    | Leq Expr Expr
    | Distinct [Expr]

-- Define command structure
data Command = 
      Declare Expr
    | Assert Formula
    | Check
    | GetMod
    | GetVal [Expr]


instance Show Expr where
  show (Val v) = show v
  show (Var v) = v
  show (Plus es) = "(+ " ++ unwords (map show es) ++ ")"
  show (Sub es) = "(- " ++ unwords (map show es) ++ ")"
  show (Mul es) = "(* " ++ unwords (map show es) ++ ")"


instance Show Formula where
  show (And fs) = "(and " ++ unwords (map show fs) ++ ")"
  show (Or fs) = "(or " ++ unwords (map show fs) ++ ")"
  show (Eq e0 e1) = "(= " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Gt e0 e1) = "(> " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Lt e0 e1) = "(< " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Geq e0 e1) = "(>= " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Leq e0 e1) = "(<= " ++ show e0 ++ " " ++ show e1 ++ ")"
  show (Distinct es) = "(distinct " ++ unwords (map show es) ++ ")"
  

instance Show Command where
  show (Declare (Var v)) = "(declare-fun " ++ v ++ " () Int)"
  show (Assert f) = "(assert " ++ show f ++ ")"
  show Check = "(check-sat)"
  show GetMod = "(get-model)"
  show (GetVal es) = "(get-value (" ++ unwords (map show es) ++ "))"


-- Killer Sudoku Solver

testFunc :: IO ()
testFunc = do
    process ["./src/KillerSudoku/cage_constraints"]

consoleFunc :: IO ()
consoleFunc = do
    -- Read cage file
    args <- getArgs
    -- Start processing to generate SMTLib file
    process args


process :: [String] -> IO ()
process [filename] = do
    -- Read cage contraints
    cageInput <- readFile filename
    putStrLn $ unlines $ map show (killerSudokuSolver cageInput)

{-
    Commands
-}

killerSudokuSolver :: String -> [Command]
killerSudokuSolver cageInput = declareVars ++ [assertConstraints cageInput] ++ checkCommands

-- Declare variables
declareVars :: [Command]
declareVars = map Declare (concat vars)

-- Assert constraints
assertConstraints :: String -> Command
assertConstraints cageInput = Assert (And generateConstraints)
    where generateConstraints = digitConstraints ++ rowConstraints ++ colConstrains ++ gridConstraints ++ cageConstraints cageInput

-- Check commands
checkCommands :: [Command]
checkCommands = [Check, GetVal $ concat vars]


{-
    Definitions
-}

-- Define length of the side
sideLength :: Int
sideLength = 9

-- Define length of a grid
gridLength :: Int
gridLength = 3

{-
    Variables
-}

-- Create variables [[x11, x12, ...], [x21, x22, ...], ...]
vars :: [[Expr]]
vars = [[Var ("x" ++ show r ++ show c) | c <- [1..sideLength]] | r <- [1..sideLength]]


{-
    Constraints
-}

-- Digit constraints
digitConstraints :: [Formula]
digitConstraints = [Leq (Val 1) v | v <- varList] ++ [Leq v (Val sideLength) | v <- varList]
    where varList = concat vars

-- Row constraints
rowConstraints :: [Formula]
rowConstraints = [Distinct row | row <- vars]

-- Column constraints
colConstrains :: [Formula]
colConstrains = [Distinct col | col <- transpose vars]

-- 3x3 grid constraints
gridConstraints :: [Formula]
gridConstraints = [Distinct grid | grid <- grids]
    where grids = [[vars!!r'!!c' | r' <- [r..(r+gridLength-1)], c' <- [c..(c+gridLength-1)]] | r <- [0,gridLength..(sideLength - 1)], c <- [0,gridLength..(sideLength - 1)]]

-- Cage constraints
cageConstraints :: String -> [Formula]
cageConstraints cageInput = [Eq (Val (read v)) (Plus (map (\x -> Var ("x" ++ x)) xs)) | (v:xs) <- cages]
    where cages = map words (lines cageInput)
