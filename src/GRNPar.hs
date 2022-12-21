{-# LANGUAGE ParallelListComp #-}

module GRNPar
    ( genNetworkSeq
    , genNetworkPar
    ) where


import Control.Monad ()

import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.List (maximumBy)

import GraphUtils (NodeState(..), BoolEdge(..), BoolNetwork(..))

import Control.Parallel.Strategies (parMap, rdeepseq, parBuffer, using)
import Control.DeepSeq ()
        
{-
instance Show BoolNetwork where
  show (BoolNetwork nodes edges) = "(" ++ intercalate ", " [name, show state] ++ ")"-}

{-
-- Testing with sample nodes:
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
-}

{-
Parallel pipeline for inferring boolean network given NodeState data.
-}
genNetworkPar :: [NodeState] -> Int -> BoolNetwork
genNetworkPar nodeStates k = BoolNetwork nodeStates (combineEdgesPar nodeStates k)

{-
Sequential pipeline for inferring boolean network given NodeState data.
-}
genNetworkSeq :: [NodeState] -> Int -> BoolNetwork
genNetworkSeq nodeStates k = BoolNetwork nodeStates (combineEdgesSeq nodeStates k)

{-
Params:
-}
combineEdgesSeq :: [NodeState] -> Int -> [BoolEdge]
combineEdgesSeq nodeStates k = concatMap (genBoolEdgesSeq nodeStates k) nodeStates

genBoolEdgesSeq :: [NodeState] -> Int -> NodeState -> [BoolEdge]
genBoolEdgesSeq nodeStates k targetNode = 
  let topKMutual = getMutualInfoSeq targetNode (filter (/= targetNode) nodeStates) k
  in map (`BoolEdge` targetNode) topKMutual

{-
Params:
-}
combineEdgesPar :: [NodeState] -> Int -> [BoolEdge]
combineEdgesPar nodeStates k = concat $ parMap rdeepseq (genBoolEdgesPar nodeStates k) nodeStates

genBoolEdgesPar :: [NodeState] -> Int -> NodeState -> [BoolEdge]
genBoolEdgesPar nodeStates k targetNode = map (`BoolEdge` targetNode) topKMutual
  where
    topKMutual = getMutualInfoPar targetNode (filter (/= targetNode) nodeStates) k 

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
    getMutualInfo' regNodes inpNodes k'
        | length regNodes == k' = regNodes
        | otherwise             =
          let (newMax, _) = maximumBy (comparing snd)
                            $ map (\inp -> (inp, mutualInformation (timeStates inp) (timeStates targetNode)
                                            - sum (map (mutualInformation (timeStates inp) . timeStates) regNodes))) inpNodes
              newInpNodes = filter (/= newMax) inpNodes
              newRegNodes = regNodes ++ [newMax]
          in getMutualInfo' newRegNodes newInpNodes k'

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
    mutualInfo'        = map (\inp -> (inp, mutualInformation (timeStates inp) (timeStates targetNode))) inputNodes `using` parBuffer 3 rdeepseq
    (maxMutualInfo, _) = maximumBy (comparing snd) mutualInfo'
    getMutualInfo' :: [NodeState] -> [NodeState] -> Int -> [NodeState]
    getMutualInfo' regNodes inpNodes k'
        | length regNodes == k' = regNodes
        | otherwise             =
          let allMutualInfo= map (\inp -> (inp, mutualInformation (timeStates inp) (timeStates targetNode)
                                            - sum (parMap rdeepseq (mutualInformation (timeStates inp) . timeStates) regNodes))) inpNodes `using` parBuffer 3 rdeepseq
              (newMax, _)   = maximumBy (comparing snd) allMutualInfo 
              newInpNodes   = filter (/= newMax) inpNodes
              newRegNodes   = regNodes ++ [newMax]
          in getMutualInfo' newRegNodes newInpNodes k'

-- Compute the entropy of a list of numerical values
entropy :: (Ord a) => [a] -> Double
entropy xs = sum [ -p * logBase 2 p | p <- ps ]
  where
    counts = Map.fromListWith (+) [(x, 1 :: Int) | x <- xs]
    values = Map.elems counts
    total = sum values
    ps = [fromIntegral v / fromIntegral total | v <- values]

-- Compute the mutual information of discrete variables x and y
mutualInformation :: (Ord a) => [a] -> [a] -> Double
mutualInformation xs ys = entropy xs + entropy ys - entropy (zip xs ys)