module Main where

import qualified AbsJetGrammar as Abs;
import qualified ParJetGrammar as Par; 

main :: IO ()
main = print ((show . Par.pTypeSystem . Par.myLexer) "rule name <- if expr1 : Int, expr2 :Int then EAdd expr1 expr2 {x->x};")
