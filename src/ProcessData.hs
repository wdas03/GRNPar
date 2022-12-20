module ProcessData
  ( csvToNodeStates
  ) where

import Control.Monad
import System.IO
import System.Environment

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Word8

import Data.List.Split
import GraphUtils (NodeState(..), BoolEdge(..), BoolNetwork(..))
import qualified Data.Matrix as M
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec

import Control.Parallel.Strategies (parMap, runEval, rdeepseq, parBuffer, withStrategy, parList, using, rseq, rpar)


-- Get CSV Data
-- Return (header, values as multi-D list) 
getCSVData :: String -> IO ([String], [[Int]])
getCSVData fname = do
    inp <- C.readFile fname
    let inp_ = C.lines inp
    if length inp_ <= 1
        then error "Invalid csv file."
        else do
            let (header:csvLines) = inp_
                headerFormatted   = map C.unpack (C.split ',' header) 
                csvLinesFormatted = map (map (\l2 -> read (C.unpack l2) :: Int) . C.split ',') csvLines
            return (headerFormatted, csvLinesFormatted)

{-
Params:
    - x: csvData returned by getCSVData
    - axis: 0 = row, 1 = column 
-}
parseCSVData :: IO ([String], [[Int]]) -> Int -> IO [NodeState]
parseCSVData x axis = do
    csvData <- x
    let (header, csvList) = csvData
        csvMatrix         = M.fromLists csvList
        nodeStates        = foldl (\acc (i, hName) -> acc ++ [NodeState hName (Vec.toList $
                                                        if axis == 0 
                                                            then M.getRow i csvMatrix 
                                                            else M.getCol i csvMatrix
                                                        )])
                                []
                                $ filter (\(_, n) -> n /= "time") (zip [0..] header)

    return nodeStates

{-
Params:
    - fname: csv filename
    - axis: 0 = row, 1 = column 

Example: 
t <- csvToNodeStates "../../test2.csv" 1
-}
csvToNodeStates :: String -> Int -> IO [NodeState]
csvToNodeStates fname = parseCSVData (getCSVData fname)
