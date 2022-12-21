{-
Main application program: 
-}

module Main (main) where

import System.IO ()
import System.Environment (getArgs)
import Control.Monad

import GRNPar (genNetworkSeq, genNetworkPar)
import GraphUtils (plotBoolNetworkPng, plotBoolNetworkPngPar, NodeState(..))
import BDDUtils (getOptimalBoolExpressions, getOptimalBoolExpressionsPar)
import ProcessData (csvToNodeStates)

import Control.DeepSeq

{-
Main script: 
    1. Infer + generate boolean network from input file
    2. Determine optimal boolean expression
    3. Output network to png file

Usage: GRNPar-exe <csvFilename> <k> <outputFile> <genExpressions> <genImage> <mode>
-}
main :: IO ()
main = do
    args <- getArgs
    case args of 
        [fname, outputFile, k, genExpressions, genImage, mode] -> do
                putStrLn fname
                putStrLn "Parsing data..."
                nodeStates@(x:_) <- csvToNodeStates fname 1
                
                -- Generate network with dependencies
                let k'         = read k :: Int
                    timeLength = length $ timeStates x
                    network    = if mode == "par" 
                                    then force $ genNetworkPar nodeStates k'
                                    else force $ genNetworkSeq nodeStates k'
                putStrLn "Generated network."

                when (genExpressions == "1") $ do
                    -- Get optimal boolean expressions for each node
                    let optimalExpressions = if mode == "par" 
                                                then force $ getOptimalBoolExpressionsPar network timeLength
                                                else force $ getOptimalBoolExpressions network timeLength
                    putStrLn "Optimal boolean expressions for each variable:"
                    mapM_ (\(n, b, c) -> putStrLn $ name n ++ " = " ++ show b ++ ". Gene-wise consistency: " ++ show c) optimalExpressions
                    --print optimalExpressions

                when (genImage == "1") $ do
                    -- Print image
                    imgFilepath <- if mode == "par"
                                     then plotBoolNetworkPngPar network outputFile False
                                     else plotBoolNetworkPng network outputFile False
                    putStrLn $ "Printed to " ++ imgFilepath ++ "."

                putStrLn "Finished."
        _       -> putStrLn "Usage: GRNPar-exe <csvFilename> <k> <outputFile> <genExpressions> <genImage> <mode>"