module Main where

import JetCodeGen
import JetIntermediateRepr
import ParJetGrammar
import ErrM
import Data.List.Split
import Data.List

run s = case (pTypeSystem . myLexer) s of
    Ok tree -> (genCode . genIntermediateRepr) tree
    Bad err -> error err

replace old new = intercalate new . splitOn old

main :: IO ()
main = do
    contents <- readFile "test/test.jt"
    let rules =  replace "\\}" "}" (replace "\\{" "{" (run contents))
    putStr rules