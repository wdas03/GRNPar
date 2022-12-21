{-# LANGUAGE TupleSections #-}

module BDDUtils
    ( BDD(..)
    , evaluateFunc
    , getOptimalBoolExpressions
    , getOptimalBoolExpressionsPar
    , getRegulatoryNodes
    , searchUpdateRule
    , getBDDFromFunc
    ) where

import GraphUtils (NodeState(..), BoolEdge(..), BoolNetwork(..))

import Data.Ord (comparing)
import Data.List (maximumBy)

import qualified Data.Matrix as M
import qualified Data.Vector as Vec

import Control.Parallel.Strategies (parMap, rdeepseq, rseq, using, parListChunk, withStrategy, parBuffer)
import Control.DeepSeq(NFData(..))

{-
Functions for evaluating and representing binary decision diagrams.
-}

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


{-
Get gene wise dynamics consistency metric given a predicted time series and the actual time series.

Params:
- predictedStates :: [Int]
- actualStates :: [Int]

-}
geneWiseDynamicsConsistency :: [Int] -> [Int] -> Double
geneWiseDynamicsConsistency predictedStates actualStates = sumPredictions / fromIntegral timeLength
  where
    sumPredictions = sum $ zipWith (\x y -> if x == y then 1 :: Double else 0 :: Double) predictedStates actualStates
    timeLength = length predictedStates

{-
Get all combinations of conjunctive and disjunctive boolean expressions.

Params: 
- n :: Int -> number of operators to evaluate

Returns: 
- [[Int]] -> list of Int lists, where each list corresponds to a sequence of AND/OR operations.

Each list, e.g. [1, 0, 1], signifies a sequence of AND/OR operators, where 1 = AND and 0 = OR

Example: given a list of truth values, such as [1, 0, 0, 1], and a boolean expression combination represented as [1, 0, 1]:
    - Using the BDD data type, we would evaluate this as: ((1 AND 0) OR 0) AND 1 
-}
getConjDisjCombos :: Int -> [[Int]]
getConjDisjCombos 1 = [[0], [1]]
getConjDisjCombos n = [[mod x 2^i | i <- [0..n-1]] | x <- [0..(2^n)-1]]


{-
Construct a BDD given boolean variable names and a combination of conjunctive/disjunctive operations.

Params:
- (x:xs) :: [String] -> boolean variable names
- ops :: [Int] -> conjunctive/disjunctive combinations as one element outputted by getConjDisjCombos

Returns: BDD

The number of variables should always be 1 more than the number of operations.

Example:
- getBDDFromFunc ["v1", "v2", "v3"] [1, 0] returns a BDD of: ((v1 AND v2) OR v3)
- bdd = getBDDFromFunc ["v1", "v2", "v3"] [1, 0]
- fromEnum $ evaluateFunc bdd [("v1", 1), ("v2", 0), ("v3", 1)]
    - Output: 1
-}
getBDDFromFunc :: [String] -> [Int] -> BDD
getBDDFromFunc [] _       = error "Invalid input."
getBDDFromFunc [x] _      = error "Invalid input."
getBDDFromFunc (x:xs) ops = foldl (\acc (y, ys) -> if ys == 1 then AND acc (Name y) else OR acc (Name y)) (Name x) tailZipped
  where
    tailZipped = zip xs ops

{-
Get regulatory genes of node given target node and boolean network.

Params:
- targetNode: target gene  
- network   : BoolNetwork
-}
getRegulatoryNodes :: NodeState -> BoolNetwork -> [NodeState]
getRegulatoryNodes targetNode network = map v_i $ filter (\(BoolEdge _ out) -> out == targetNode) $ connections network

{-
Compute genewise dynamics consistency metric for every possible boolean expression with input nodes mapping to target node.
-}
searchUpdateRule :: [NodeState] -> NodeState -> Int -> [(Double, BDD)]
searchUpdateRule inpNodes targetNode timeLength = map (\(p, r) -> (geneWiseDynamicsConsistency p targetStates, r)) predStates
  where
    -- Get target states of target node
    (_:targetStates) = timeStates targetNode

    inpNodeNames = map name inpNodes
    inpMatrix    = M.fromLists $ map (\xs -> map (name xs,) (timeStates xs)) inpNodes

    -- Get boolean expression combos
    ruleCombos = map (getBDDFromFunc inpNodeNames) $ getConjDisjCombos (length inpNodes - 1)

    -- Predict states using boolean expression
    predStates   = map (\ruleBDD ->
                        let p = map (fromEnum . \t -> evaluateFunc ruleBDD (filter (\(nn, _) -> nn `elem` inpNodeNames)
                                              $ Vec.toList (M.getCol t inpMatrix)))
                                              [1..(timeLength - 1)]
                        in (p, ruleBDD))
                        ruleCombos

searchUpdateRulePar :: [NodeState] -> NodeState -> Int -> [(Double, BDD)]
searchUpdateRulePar inpNodes targetNode timeLength = map (\(p, r) -> (geneWiseDynamicsConsistency p targetStates, r)) predStates
  where
    ruleCombos       = parMap rdeepseq (getBDDFromFunc inpNodeNames) (getConjDisjCombos (length inpNodes))
    (_:targetStates) = timeStates targetNode
    inpNodeNames     = map name inpNodes
    inpMatrix        = M.fromLists $ map (\xs -> map (name xs,) (timeStates xs) `using` parBuffer 50 rdeepseq) inpNodes
    predStates       = parMap rdeepseq (\ruleBDD ->
                            let p = map (fromEnum . \t -> evaluateFunc ruleBDD (filter (\(nn, _) -> nn `elem` inpNodeNames)
                                    $ Vec.toList  (M.getCol t inpMatrix)))
                                    [1..(timeLength - 1)] 
                                    `using` parBuffer 50 rdeepseq
                            in (p, ruleBDD))
                            ruleCombos

{-
Get optimal boolean expressions for each node in network that optimizes genewise dynamics consistency.
-}
getOptimalBoolExpressions :: BoolNetwork -> Int -> [(NodeState, BDD, Double)]
getOptimalBoolExpressions inferredNetwork timeLength = map (\targetNode -> 
                                                            let inpNodes                     = getRegulatoryNodes targetNode inferredNetwork
                                                                -- Get consistency metrics for each boolean expression
                                                                consistencyMetrics           = searchUpdateRule inpNodes targetNode timeLength
                                                                -- Get boolean expression with max consistency metric
                                                                (maxConsistency, optimalBDD) = maximumBy (comparing fst) consistencyMetrics
                                                            in (targetNode, optimalBDD, maxConsistency)) 
                                                          $ nodes inferredNetwork

getOptimalBoolExpressionsPar :: BoolNetwork -> Int -> [(NodeState, BDD, Double)]
getOptimalBoolExpressionsPar inferredNetwork k = map (\targetNode -> 
                                                        let inpNodes                     = getRegulatoryNodes targetNode inferredNetwork
                                                            consistencyMetrics           = searchUpdateRulePar inpNodes targetNode k
                                                            (maxConsistency, optimalBDD) = maximumBy (comparing fst) consistencyMetrics
                                                        in (targetNode, optimalBDD, maxConsistency)) 
                                                 (nodes inferredNetwork)
                                                 `using` parListChunk 50 rdeepseq

-- sample nodes
x_1 :: NodeState
x_1 = NodeState "x_1" [1, 1, 0, 0, 0, 1, 0, 1]

x_2 :: NodeState
x_2 = NodeState "x_2" [1, 0, 1, 0, 1, 0, 1, 0]

x_3 :: NodeState
x_3 = NodeState "x_3" [1, 0, 1, 0, 1, 0, 1, 0]

x_4 :: NodeState
x_4 = NodeState "x_4" [1, 0, 0, 0, 1, 0, 1, 1]

x_5 :: NodeState
x_5 = NodeState "x_5" [1, 0, 1, 0, 0, 0, 0, 0]

x_6 :: NodeState
x_6 = NodeState "x_6" [1, 1, 0, 0, 1, 1, 1, 0]

sampleNodes :: [NodeState]
sampleNodes = [x_1,x_2,x_3,x_4,x_5,x_6]
