module Main where

import qualified AbsJetGrammar as Abs;
import qualified ParJetGrammar as Par; 

main :: IO ()
main = print ((show . Par.pTypeRule . Par.myLexer) "rule Ident -> Ident : Int | Ident : Int")
