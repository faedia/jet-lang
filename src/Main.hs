module Main where

import JetCodeGen
import JetIntermediateRepr
import ParJetGrammar
import ErrM
import System.Environment
import System.Exit
import Data.List (elemIndex)



run :: String -> Bool -> String
run s tr = case (pTypeSystem . myLexer) s of
    Ok tree -> (genCode tr . genIntermediateRepr) tree
    Bad err -> error err

printHelp = putStr ("Usage: jetgen [-t] [-o file] file\n" 
    ++ "-o file     Outputs the generated source code to this file\n"
    ++ "-t          Enables tracing of tree traversal in generated code\n"
    ++ "-h          Prints this message\n")

parseArgs :: [String] -> IO (String, String, Bool)
parseArgs [] = do 
    printHelp 
    exitFailure
parseArgs argv = 
    if elem "-h" argv then parseArgs []
    else let tr = elem "-t" argv in
        case elemIndex "-o" argv of
            Just idx    | idx + 1 < length argv - 1 -> let outfile = argv !! (idx + 1) in
                return (last argv, outfile, tr)
            Nothing -> return (last argv, "-", tr)
            

main :: IO ()
main = do
    args <- getArgs
    (inFile, outFile, tr) <- parseArgs args
    contents <- readFile inFile
    let code = run contents tr
    if outFile == "-" then putStrLn code
    else writeFile outFile code