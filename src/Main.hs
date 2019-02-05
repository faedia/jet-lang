module Main where

import qualified AbsJetGrammar as Abs;
import qualified ParJetGrammar as Par;
import qualified ErrM;
import Data.List;
import Data.Char;

isTypeVar :: Abs.Ident -> Bool
isTypeVar (Abs.Ident str) = isLower (head str)

identToString :: Abs.Ident -> String
identToString (Abs.Ident str) = str

genCheckRule :: Abs.TypeRule -> ErrM.Err String
genCheckRule (Abs.TRule name premises judgement scond) = case premises of
    Abs.TNoPremis -> case judgement of
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars Abs.TNone -> if null vars then
                ErrM.Ok ("check" ++ prodName ++ " ctx t " ++ labelName ++ " = Ok ctx")
            else
                ErrM.Bad "If the current node has no children then we should have no premises"
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars (Abs.TType tvar tparams) ->
            if isTypeVar tvar then
                if null tparams then 
                    ErrM.Ok "Need to do something to get the type var"
                else
                    ErrM.Bad "Type Vars don't take params"
            else
                ErrM.Ok ("check" ++ prodName ++ " ctx t (" ++ labelName ++ foldr (\a b -> b ++ " " ++ identToString a) "" vars ++ ") = if t == " ++ identToString tvar ++ foldr (\a b -> b ++ " " ++ identToString a) "" tparams ++ " then Ok ctx else Bad \"Inconsistent types\"")
    _ -> ErrM.Ok "TODO"

genInferRule :: Abs.TypeRule -> ErrM.Err String
genInferRule rule = ErrM.Ok "Infer"

generateFromRule :: Abs.TypeRule -> ErrM.Err [String]
generateFromRule (Abs.TRule rname tpremise (Abs.TJudge prod label params t) scond) = do
    let rule = (Abs.TRule rname tpremise (Abs.TJudge prod label params t) scond)
    checkRule <- genCheckRule rule
    inferRule <- genInferRule rule
    case t of
        Abs.TNone -> ErrM.Ok [checkRule]
        _ -> ErrM.Ok (checkRule : [inferRule])

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

gen :: [Abs.TypeRule] -> ErrM.Err [String]
gen [] = ErrM.Ok []
gen (r:rs) = do
    rgen <- generateFromRule r
    rsgen <- gen rs
    ErrM.Ok (rgen ++ rsgen)

run :: String ->  ErrM.Err [String]
run s = case (Par.pTypeSystem . Par.myLexer) s of
    ErrM.Bad err -> ErrM.Bad err
    ErrM.Ok tree -> do
        let rules = getAllRules tree
        gen rules

main :: IO ()
main = do
    contents <- readFile "test.txt"
    let rules = run contents
    case rules of
        ErrM.Bad err -> print err
        ErrM.Ok rs -> do_print rs
        where
            do_print (x1:x2:xs) = do
                print x1
                do_print (x2:xs)
            do_print [x1] = print x1