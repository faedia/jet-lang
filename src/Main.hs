module Main where

import qualified AbsJetGrammar as Abs;
import qualified ParJetGrammar as Par;
import qualified ErrM;
import Data.List;

getRuleName :: Abs.TypeRule -> String
getRuleName (Abs.TRule (Abs.RName (Abs.Ident x)) _ _ _) = x

getAllRules :: Abs.TypeSystem -> [Abs.TypeRule]
getAllRules (Abs.TSystem rules) = rules

knownRuleNames :: [Abs.TypeRule] -> ErrM.Err [String]
knownRuleNames rules = let names = map getRuleName rules in
    if length names == (length . nub) names then
        ErrM.Ok names
    else
        ErrM.Bad "Duplicate value"

run :: String ->  ErrM.Err [Abs.TypeRule]
run s = case (Par.pTypeSystem . Par.myLexer) s of
    ErrM.Bad err -> ErrM.Bad err
    ErrM.Ok tree -> ErrM.Ok (getAllRules tree)

main :: IO ()
main = do
    let rules = run "rule add <- if Expr : t, Expr : t then Expr (EAdd expr1 expr2) : t; rule nat <- Expr (ENum num) : Int; rule add <- Expr (EVar var) : t;"
    case rules of
        ErrM.Bad err -> print err
        ErrM.Ok rs -> do_print rs
        where
            do_print (x1:x2:xs) = do
                print x1
                do_print (x2:xs)
            do_print [x1] = print x1