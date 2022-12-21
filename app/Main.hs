{-
Main application program: 
-}

module Main (main) where

import System.IO ()
import System.Environment (getArgs)
import Control.Monad

import Data.List (intercalate)

import GRNPar (genNetworkSeq, genNetworkPar)
import GraphUtils (plotBoolNetworkPng, NodeState(..))
import BDDUtils (getOptimalBoolExpressions)
import ProcessData (csvToNodeStates)

-- Main loop
-- Usage: grnPAR-exe <csvFilename> <k> <outputFile> <mode> 
main :: IO ()
main = do
    args <- getArgs
    case args of 
        [fname, outputFile, k, mode, genExpressions, genImage] -> do
                putStrLn fname
                putStrLn "Parsing data..."
                nodeStates <- csvToNodeStates fname 1
                
                -- Generate network with dependencies
                let k'      = read k :: Int
                    network = if mode == "par" 
                                then genNetworkPar nodeStates k'
                                else genNetworkSeq nodeStates k'
                putStrLn "Generated network."

                when (genExpressions == "1") $ do
                    -- Get optimal boolean expressions for each node
                    let optimalExpressions = getOptimalBoolExpressions network k'
                    putStrLn "Optimal boolean expressions for each variable:"
                    mapM_ (\(n, b, c) -> putStrLn $ name n ++ ": " ++ intercalate "," [show b, show c]) optimalExpressions
                    --print optimalExpressions

                when (genImage == "1") $ do
                    -- Print image
                    imgFilepath <- plotBoolNetworkPng network outputFile False
                    putStrLn $ "Printed to " ++ imgFilepath ++ "."

                putStrLn "Finished."
        _           -> putStrLn "Usage: grnPAR-exe <csvFilename> <k> <outputFile> <mode> <genExpressions> <genImage>"