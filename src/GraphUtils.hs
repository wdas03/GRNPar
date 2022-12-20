module GraphUtils
    ( NodeState(..)
    , BoolEdge(..)
    , BoolNetwork(..)
    , getRegulatoryNodes
    , plotBoolNetworkPng
    , plotDGPng
    ) where

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Hashable

import Control.DeepSeq ( NFData(..) )

import qualified Data.Text.Lazy    as TL
import qualified Data.Graph.DGraph as DG
import qualified Data.Graph.Types  as GT

{-
Data types representing boolean states of genes and boolean network consisting of NodeStates and directed edges.

- NodeState:
    - name :: String -> name of gene
    - timeStates :: [Int] -> expression of gene across time-series in order

- BoolEdge
    - v_i :: NodeState -> source node
    - v_j :: NodeState -> target node

- BoolNetwork
    - nodes :: [NodeState] -> all nodes/genes in network
    - connections :: [BoolEdge] -> directed edges between genes
-}

data NodeState = NodeState { name :: String
                           , timeStates :: [Int] } deriving (Eq, Ord)
instance Show NodeState where
    show (NodeState n _) = n 
instance NFData NodeState where
    rnf (NodeState n ts) = rnf n `seq` rnf ts

data BoolEdge = BoolEdge { v_i :: NodeState
                         , v_j :: NodeState } deriving (Eq)
instance Show BoolEdge where
    show (BoolEdge i j) = "(" ++ name i ++ " -> " ++ name j ++ ")" 
instance NFData BoolEdge where
    rnf (BoolEdge i j) = rnf i `seq` rnf j

data BoolNetwork = BoolNetwork { nodes :: [NodeState]
                               , connections :: [BoolEdge] } deriving (Eq, Show)   
               

{-
Get regulatory genes of node given target node and boolean network.

Params:
- targetNode: target gene  
- network   : BoolNetwork
-}
getRegulatoryNodes :: NodeState -> BoolNetwork -> [NodeState]
getRegulatoryNodes targetNode network = map v_i $ filter (\(BoolEdge _ out) -> out == targetNode) $ connections network

{-
Modified from graphite lib source code:
- Functions for plotting directed graph and BoolNetwork to png file
- graphviz needs to be installed on local machine to see output: 
    - brew install graphviz
        OR
    - sudo apt install graphviz

Example: Plot DGraph foundationUniverse to "foundation.png"

foundationUniverse :: DG.DGraph String Double
foundationUniverse = DG.fromArcsList
    [ GT.Arc "Helicon" "Nishaya" 200.00
    , GT.Arc "Helicon" "Wencory" 382.20
    , GT.Arc "Nishaya" "Wencory" 820.32
    ]

plotDGraphPng foundationUniverse "foundation"

-}

-- Plot a BoolNetwork to png file
boolNetworkToDG :: BoolNetwork -> Bool -> DG.DGraph String String
boolNetworkToDG network labelEdges = DG.fromArcsList 
                                    $ map (\(BoolEdge inp out) -> 
                                        GT.Arc (name inp) (name out) (if labelEdges then "test" else "")) 
                                        (connections network) 

plotBoolNetworkPng :: BoolNetwork -> FilePath -> Bool -> IO FilePath
plotBoolNetworkPng network fname labelEdges = plotDGPng networkDG fname labelEdges
  where
    networkDG = boolNetworkToDG network labelEdges

-- | Plot a directed 'DGraph' to a PNG image file
plotDGPng :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => DG.DGraph v e
 -> FilePath
 -> Bool
 -> IO FilePath
plotDGPng g fname labelEdges = addExtension (runGraphvizCommand Sfdp $ toDirectedDot labelEdges g) Png fname

labeledNodes :: (GT.Graph g, Show v) => g v e -> [(v, String)]
labeledNodes g = (\v -> (v, show v)) <$> GT.vertices g

labeledArcs :: (Hashable v, Eq v, Show e) => DG.DGraph v e -> [(v, v, String)]
labeledArcs g = (\(GT.Arc v1 v2 attr) -> (v1, v2, show attr)) <$> DG.arcs g

toDirectedDot :: (Hashable v, Ord v, Show v, Show e)
 => Bool -- ^ Label edges
 -> DG.DGraph v e
 -> DotGraph v
toDirectedDot labelEdges g = graphElemsToDot params (labeledNodes g) (labeledArcs g)
    where params = sensibleDotParams True labelEdges

sensibleDotParams
 :: Bool -- ^ Directed
 -> Bool -- ^ Label edges
 -> GraphvizParams t l String () l
sensibleDotParams directed edgeLabeled = nonClusteredParams
    { isDirected = directed
    , globalAttributes =
        [ GraphAttrs [Overlap ScaleOverlaps]
        , EdgeAttrs [FontColor (X11Color DarkGreen)]
        ]
    , fmtEdge = edgeFmt
    }
    where
        edgeFmt (_, _, l) = if edgeLabeled
            then [Label $ StrLabel $ TL.pack l]
            else []