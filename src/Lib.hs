{-# LANGUAGE TupleSections #-}

module Lib
    ( runScript
    ) where

import qualified Data.Map as Map
import Control.Monad
import System.IO
import System.Environment
import Data.Ord (comparing)
import Data.Map (toList, fromListWith)
import Data.Maybe ( maybeToList, fromJust, fromMaybe, catMaybes)
import Data.List ( find, intercalate, maximumBy)

import GraphUtils (getRegulatoryNodes, plotDGPng, plotBoolNetworkPng, NodeState(..), BoolEdge(..), BoolNetwork(..))
import ProcessData (csvToNodeStates)
import BDDUtils (evaluateFunc, BDD(..))

import qualified Data.Graph.DGraph as DG
import qualified Data.Graph.Types  as GT
import qualified Data.Graph.UGraph as UG

import qualified Data.Matrix as M
import qualified Data.Maybe

import qualified Data.Vector as Vec

import Control.Parallel.Strategies (parMap, runEval, parListChunk, rdeepseq, parBuffer, withStrategy, parList, using, rseq, rpar)
import Control.DeepSeq

-- Get list 
runScript :: IO ()
runScript = do
    args <- getArgs
    if length args == 4
        then do
            let (fname:outputFile:k:mode:_) = args
            putStrLn "Parsing data..."
            nodeStates <- csvToNodeStates fname 1
            let network = if mode == "par" 
                            then genNetworkPar nodeStates (read k :: Int) 
                            else genNetworkSeq nodeStates (read k :: Int)
            plotBoolNetworkPng network outputFile False
            return ()
            --print network
    else do
        putStrLn "Usage: grnPAR-exe <csvFilename> <k> <outputFile> <mode>"

{-
instance Show BoolNetwork where
  show (BoolNetwork nodes edges) = "(" ++ intercalate ", " [name, show state] ++ ")"-}

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

-- delete item from list
delete :: Eq a => a -> [a] -> [a]
delete deleted xs = [ x | x <- xs, x /= deleted ]

-- define a function to compute the entropy of a list of values
entropy :: (Ord a) => [a] -> Double
entropy xs = sum [ -p * logBase 2 p | p <- ps ]
  where
    counts = Map.fromListWith (+) [(x, 1) | x <- xs]
    values = Map.elems counts
    total = sum values
    ps = [fromIntegral v / fromIntegral total | v <- values]

-- define a function to compute the mutual information of discrete variables x and y
mutualInformation :: (Ord a) => [a] -> [a] -> Double
mutualInformation xs ys = entropy xs + entropy ys - entropy (zip xs ys)

-- max index function
maxIndex ::  Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

{-
Params:
-}
combineEdgesSeq :: [NodeState] -> Int -> [BoolEdge]
combineEdgesSeq nodeStates k = concatMap (genBoolEdgesSeq nodeStates k) nodeStates

genBoolEdgesSeq :: [NodeState] -> Int -> NodeState -> [BoolEdge]
genBoolEdgesSeq nodeStates k targetNode = 
  let topKMutual = getMutualInfoSeq targetNode (filter (/= targetNode) nodeStates) k
  in map(`BoolEdge` targetNode) topKMutual

{-
Params:
-}
combineEdgesPar :: [NodeState] -> Int -> [BoolEdge]
combineEdgesPar nodeStates k = concat (map (genBoolEdgesPar nodeStates k) nodeStates `using` parListChunk 64 rpar)

genBoolEdgesPar :: [NodeState] -> Int -> NodeState -> [BoolEdge]
genBoolEdgesPar nodeStates k targetNode = 
  let topKMutual = getMutualInfoPar targetNode (filter (/= targetNode) nodeStates) k
  in map (`BoolEdge` targetNode) topKMutual `using` parListChunk 64 rpar

genNetworkPar :: [NodeState] -> Int -> BoolNetwork
genNetworkPar nodeStates k = BoolNetwork nodeStates (combineEdgesPar nodeStates k)

{-
Params:
- nodeStates:
- k         :
-}
genNetworkSeq :: [NodeState] -> Int -> BoolNetwork
genNetworkSeq nodeStates k = BoolNetwork nodeStates (combineEdgesSeq nodeStates k)

{-
Get top k nodes with the highest mutual information relative to a target node.

Params:
- targetNode:
- inputNodes:
- k         :

* inputNodes should not contain targetNode when calling getMutualInfo
-}
getMutualInfoSeq :: NodeState -> [NodeState] -> Int -> [NodeState]
getMutualInfoSeq targetNode inputNodes k = getMutualInfo' [maxMutualInfo] (filter (/= maxMutualInfo) inputNodes) k
  where
    (maxMutualInfo, _) = maximumBy (comparing snd) $ map (\inp -> (inp, mutualInformation (timeStates inp) (timeStates targetNode))) inputNodes
    getMutualInfo' :: [NodeState] -> [NodeState] -> Int -> [NodeState]
    getMutualInfo' regNodes inpNodes k
        | length regNodes == k = regNodes
        | otherwise            =
          let (newMax, _) = maximumBy (comparing snd)
                            $ map (\inp -> (inp, mutualInformation (timeStates inp) (timeStates targetNode)
                                            - sum (map (mutualInformation (timeStates inp) . timeStates) regNodes))) inpNodes
              newInpNodes = filter (/= newMax) inpNodes
              newRegNodes = regNodes ++ [newMax]
          in getMutualInfo' newRegNodes newInpNodes k

{-
Get top k nodes with the highest mutual information relative to a target node.

Params:
- targetNode:
- inputNodes:
- k         :

* inputNodes should not contain targetNode when calling getMutualInfo
-}
getMutualInfoPar :: NodeState -> [NodeState] -> Int -> [NodeState]
getMutualInfoPar targetNode inputNodes k = getMutualInfo' [maxMutualInfo] (filter (/= maxMutualInfo) inputNodes) k
  where
    (maxMutualInfo, _) = maximumBy (comparing snd) $ parMap rdeepseq (\inp -> (inp, mutualInformation (timeStates inp) (timeStates targetNode))) inputNodes
    getMutualInfo' :: [NodeState] -> [NodeState] -> Int -> [NodeState]
    getMutualInfo' regNodes inpNodes k
        | length regNodes == k = regNodes
        | otherwise            =
          let allMutualInfo= parMap rdeepseq (\inp -> (inp, mutualInformation (timeStates inp) (timeStates targetNode)
                                            - sum (parMap rdeepseq (mutualInformation (timeStates inp) . timeStates) regNodes))) inpNodes 
              (newMax, _)   = maximumBy (comparing snd) allMutualInfo 
              newInpNodes   = filter (/= newMax) inpNodes
              newRegNodes   = regNodes ++ [newMax]
          in getMutualInfo' newRegNodes newInpNodes k

{-
Get gene wise dynamics consistency metric given a predicted time series and the actual time series.

Params:
- predictedStates :: [Int]
- actualStates :: [Int]

-}
geneWiseDynamicsConsistency :: [Int] -> [Int] -> Double
geneWiseDynamicsConsistency predictedStates actualStates = fromIntegral sumPredictions / fromIntegral timeLength
  where
    sumPredictions = sum $ zipWith (\x y -> if x == y then 1 else 0) predictedStates actualStates
    timeLength = length predictedStates


{-
Get all combinations of conjunctive and disjunctive boolean expressions as a list of lists.

Params: 
- n :: Int -> number of operators to evaluate

Each list, e.g. [1, 0, 1], signifies a sequence of AND/OR operators, where 1 = AND and 0 = OR

Example: given a list of truth values, such as [1, 0, 0, 1], and a boolean expression combination represented as [1, 0, 1]:
    - Using the BDD data type, we would evaluate this as: ((1 AND 0) OR 0) AND 1 
-}
getConjDisjCombos :: Int -> [[Int]]
getConjDisjCombos n = [[mod x 2^i | i <- [0..n-1]] | x <- [0..(2^n)-1]]


{-
Construct a BDD given boolean variable names and a combination of conjunctive/disjunctive operations.

Params:
- (x:xs) :: [String] -> boolean variable names
- ops :: [Int] -> conjunctive/disjunctive combinations as one element outputted by getConjDisjCombos

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
getBDDFromFunc (x:xs) ops = foldl (\acc (xs, ys) -> if ys == 1 then AND acc (Name xs) else OR acc (Name xs)) (Name x) tailZipped
  where
    tailZipped = zip xs ops

{-

-}
--searchUpdateRule :: [NodeState] -> NodeState -> Int -> Double
searchUpdateRule inpNodes targetNode timeLength = map (\(p, r) -> (geneWiseDynamicsConsistency p targetStates, r)) predStates
  where
    ruleCombos   = map (getBDDFromFunc inpNodeNames) $ getConjDisjCombos (length inpNodes)
    (_:targetStates) = timeStates targetNode
    inpNodeNames = map name inpNodes
    inpMatrix    = M.fromLists $ map (\xs -> map (name xs,) (timeStates xs)) sampleNodes
    predStates   = map (\ruleBDD ->
                        let pred = map (fromEnum . \t -> evaluateFunc ruleBDD (filter (\(nn, _) -> nn `elem` inpNodeNames)
                                              $ Vec.toList  (M.getCol t inpMatrix)))
                                              [1..(timeLength - 1)]
                        in (pred, ruleBDD))
                        ruleCombos

{-
Sequential pipeline for inferring boolean network given NodeState data.

-}

{-
swapRoutine :: NodeState -> [NodeState] -> [NodeState] -> ([NodeState], String)
swapRoutine targetNode selectedInpNodes unselectedNodes = 
    map (\un -> map (\sn -> (filter (/= sn) selectedNodes) ++ un) selectedNodes) unselectedNodes
    where 
      maxC = searchUpdateRule selectedNodes targetNode
      swapRoutine' target sInp uInp
        | 
-}

{-
mifs :: NodeState -> [NodeState] -> Int -> [NodeState]
mifs v_o allNodes iterations =
    let removed = delete v_o allNodes
        v =  maxIndex $ map (\x -> mutualInformation (timeStates x) (timeStates v_o)) removed
        new_stack = [removed !! v]
        old_words = takeWhile (/= (allNodes !! v)) removed
    in mifs' new_stack old_words iterations
    where
      mifs' :: [NodeState] -> [NodeState] -> Int -> [NodeState]
      mifs' s_list w_list k
        | length s_list < k =
          let dependency w = mutualInformation (timeStates v_o) (timeStates w) - sumValue w
              sumValue w = sum(map (mutualInformation (timeStates w) . timeStates) w_list)
              v'           = maxIndex $ map dependency w_list
              s_list'      = s_list ++ [w_list !! v']
              w_list'      = delete (w_list !! v') w_list
          in mifs' s_list' w_list' k
        | otherwise = s_list

createNetwork :: [NodeState] -> BoolNetwork
createNetwork nodes = BoolNetwork nodes $ map (\v_o -> updateNet v_o (mifs v_o nodes)) nodes
  where 
    updateNet :: NodeState -> [NodeState] -> [Edge] -> [Edge]
    updateNet curNode (x:_) allEdges = allEdges ++ [Edge curNode x]
    updateNet curNode (x:xs) allEdges = updateNet curNode xs (allEdges ++ [Edge curNode x])
    -}