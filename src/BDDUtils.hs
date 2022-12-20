module BDDUtils
    ( BDD(..)
    , evaluateFunc
    ) where

import Control.DeepSeq(NFData(..))

{-
Binary decision diagram data type used for evaluating boolean expressions.

Example:
- f = XOR (AND (Name "x") (OR (Name "y") (Name "a"))) (NOT (Name "z"))
- fromEnum $ evaluateFunc f [("x", 1), ("y", 0), ("a", 1), ("z", 1)]
  - Output: 1
-}

-- Define binary decision diagram
data BDD = Name String  -- name of node (gene name)
         | State Int    -- state of node (0 or 1)
         | AND BDD BDD  -- an AND node
         | OR  BDD BDD  -- an OR node
         | XOR BDD BDD  -- an XOR node
         | NOT BDD      -- a NOT node
         deriving (Eq)

instance Show BDD where
  show (Name x)  = x
  show (State x) = show x
  show (AND x y) = "(" ++ show x ++ " AND " ++ show y ++ ")"
  show (OR x y)  = "(" ++ show x ++ " OR " ++ show y ++ ")"
  show (XOR x y) = "(" ++ show x ++ " XOR " ++ show y ++ ")"
  show (NOT x)   = "(NOT " ++ show x ++ ")"

instance NFData BDD where
  rnf (Name x)  = rnf x
  rnf (State x) = rnf x
  rnf (AND x y) = rnf x `seq` rnf y
  rnf (OR x y)  = rnf x `seq` rnf y
  rnf (XOR x y) = rnf x `seq` rnf y
  rnf (NOT x)   = rnf x 

{-
Evaluate a boolean expression given a BDD data type and a list of tuples specifying values for each variable.

Params:
- BDD data type
- func: list of tuples [(varName, value)], where varName is a boolean variable name in the BDD, and value is the value assigned to it

Example:
- f = XOR (AND (Name "x") (OR (Name "y") (Name "a"))) (NOT (Name "z"))
- fromEnum $ evaluateFunc f [("x", 1), ("y", 0), ("a", 1), ("z", 1)]
  - Output: 1

-}
-- evaluate a BDD for a given assignment of the variables
evaluateFunc :: BDD -> [(String, Int)] -> Bool
evaluateFunc (Name x) func = case lookup x func of
                                Just a  -> a == 1
                                Nothing -> error "Variable not in assignment"
evaluateFunc (AND x y) func = evaluateFunc x func && evaluateFunc y func
evaluateFunc (OR x y) func = evaluateFunc x func || evaluateFunc y func
evaluateFunc (XOR x y) func
  | andTrue   = False
  | otherwise = evaluateFunc (OR x y) func
  where
    andTrue = evaluateFunc x func && evaluateFunc y func
evaluateFunc (NOT b) func = not (evaluateFunc b func)
evaluateFunc (State x) func
    | x == 1    = True
    | otherwise = False