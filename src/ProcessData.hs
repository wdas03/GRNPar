module ProcessData
  ( csvToNodeStates
  ) where

import qualified Data.ByteString.Char8 as C

import GraphUtils (NodeState(..))
import qualified Data.Matrix as M
import qualified Data.Vector as Vec


-- Get CSV Data
-- Return (header, values as multi-D list) 
getCSVData :: String -> IO ([String], [[Int]])
getCSVData fname = do
    inp <- C.readFile fname
    let inpLines = C.lines inp
    case inpLines of 
        []                -> error "Invalid csv file."
        [_]               -> error "Invalid csv file."
        (header:csvLines) -> do
            let headerFormatted   = map C.unpack (C.split ',' header) 
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
