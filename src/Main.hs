{-# Language TemplateHaskell #-}

module Main where

import JetCodeGen
import JetIntermediateRepr
import ParJetGrammar
import ErrM
import System.Environment
import System.Exit
import Data.List (elemIndex)
import System.FilePath (replaceDirectory, replaceExtension, takeBaseName)
import System.Directory (createDirectoryIfMissing)
import Data.FileEmbed (embedFile)
import qualified Data.ByteString as B

jetContextFile = $(embedFile "common/JetContext.hs")
jetErrorFile = $(embedFile "common/JetErrorM.hs")

run :: String -> String -> Bool -> String
run name s tr = case (pTypeSystem . myLexer) s of
    Ok tree -> (genCode name tr . genIntermediateRepr) tree
    Bad err -> error err

printHelp = putStr ("Usage: jetgen [-t] [-o file] file\n" 
    ++ "-o dir      Outputs the generated source code to the specified directory\n"
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
            Just idx    | idx + 1 < length argv - 1 -> let outdir = argv !! (idx + 1) in
                return (last argv, outdir, tr)
            Nothing -> return (last argv, "./", tr)
            

main :: IO ()
main = do
    args <- getArgs
    (inFile, outDir, tr) <- parseArgs args
    contents <- readFile inFile
    let code = run (takeBaseName inFile) contents tr
    let outFile = replaceDirectory (replaceExtension inFile ".hs") outDir
    createDirectoryIfMissing True outDir
    writeFile outFile code
    B.writeFile (replaceDirectory "JetContext.hs" outDir) jetContextFile
    B.writeFile (replaceDirectory "JetErrorM.hs" outDir) jetErrorFile