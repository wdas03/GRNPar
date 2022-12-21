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
import BDDUtils (getOptimalBoolExpressions, getOptimalBoolExpressionsPar)
import ProcessData (csvToNodeStates)

-- Main loop
-- Usage: GRNPar-exe <csvFilename> <k> <outputFile> <genExpressions> <genImage> <mode>
main :: IO ()
main = do
    args <- getArgs
    case args of 
        [fname, outputFile, k, genExpressions, genImage, mode] -> do
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
                    let optimalExpressions = if mode == "par" 
                                                then getOptimalBoolExpressionsPar network k' 
                                                else getOptimalBoolExpressions network k'
                    putStrLn "Optimal boolean expressions for each variable:"
                    mapM_ (\(n, b, c) -> putStrLn $ name n ++ ": " ++ intercalate "," [show b, show c]) optimalExpressions
                    --print optimalExpressions

                when (genImage == "1") $ do
                    -- Print image
                    imgFilepath <- plotBoolNetworkPng network outputFile False
                    putStrLn $ "Printed to " ++ imgFilepath ++ "."

                putStrLn "Finished."
        _       -> putStrLn "Usage: GRNPar-exe <csvFilename> <k> <outputFile> <genExpressions> <genImage> <mode>"