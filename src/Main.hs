module Main where

import JetCodeGen
import JetIntermediateRepr
import ParJetGrammar
import ErrM

run :: String -> String
run s = case (pTypeSystem . myLexer) s of
    Ok tree -> (genCode . genIntermediateRepr) tree
    Bad err -> error err

main :: IO ()
main = do
    contents <- readFile "test/test.jt"
    let rules = run contents
    putStr rules