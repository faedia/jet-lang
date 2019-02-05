module Main where

import qualified AbsJetGrammar as Abs;
import qualified ParJetGrammar as Par;
import qualified ErrM;
import Data.List;
import Data.Char;

isTypeVar :: Abs.Ident -> Bool
isTypeVar (Abs.Ident str) = isLower (head str)

getInlineHaskell :: Abs.InlineHaskell -> String
getInlineHaskell (Abs.InlineHaskell str) = (tail . init) str

ctxDefnCode :: Abs.ContextDefn -> String
ctxDefnCode (Abs.CtxDefn code) = getInlineHaskell code

identToString :: Abs.Ident -> String
identToString (Abs.Ident str) = str

checkFunctionPatternDefn :: Abs.TypeJudgement -> String
checkFunctionPatternDefn (Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars _) = "check" ++ prodName ++ " ctx jetCheckType (" ++ labelName ++ foldr (\a b -> b ++ " " ++ identToString a) "" vars ++ ") = "

sideCond :: Abs.SideCondition -> String
sideCond (Abs.HSideCondition scond) = getInlineHaskell scond
sideCond Abs.HEmptySideCondition = ""

hasSideCond :: Abs.SideCondition -> Bool
hasSideCond Abs.HEmptySideCondition = False
hasSideCond _ = True

getT :: Abs.Ident -> [Abs.Ident] -> Abs.SideCondition -> String
getT tvar tparams scond = if isTypeVar tvar then
    sideCond scond
    else
        "let t = " ++ identToString tvar ++ foldr (\a b -> b ++ " " ++ identToString a) "" tparams ++ (
            if hasSideCond scond then 
                "\n\t" ++ sideCond scond 
            else 
                "")

genCheckRule :: Abs.TypeRule -> ErrM.Err String
genCheckRule (Abs.TRule name premises ctx judgement scond) = case premises of
    Abs.TNoPremis -> case judgement of
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars Abs.TNone -> if null vars then
                ErrM.Ok (checkFunctionPatternDefn judgement ++ " = Ok " ++ ctxDefnCode ctx)
            else
                ErrM.Bad "If the current node has no children then we should have no premises"
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars (Abs.TType tvar tparams) ->
            let t' = getT tvar tparams scond in
                if isTypeVar tvar then
                    if null tparams then
                        ErrM.Ok (checkFunctionPatternDefn judgement ++ "do\n\t" ++ t' ++ "\n\tif jetCheckType == t then Ok (" ++ ctxDefnCode ctx ++ ") else Bad \"Inconsistent types\"")
                    else
                        ErrM.Bad "Type Vars don't take params"
                else
                    ErrM.Ok (checkFunctionPatternDefn judgement ++ "do\n\t" ++ t' ++ "\n\tif jetCheckType == t then Ok (" ++ ctxDefnCode ctx ++ ") else Bad \"Inconsistent types\"")
    _ -> ErrM.Ok "TODO: Premises"

genInferRule :: Abs.TypeRule -> ErrM.Err String
genInferRule rule = ErrM.Ok "TODO: Infer"

generateFromRule :: Abs.TypeRule -> ErrM.Err [String]
generateFromRule (Abs.TRule rname tpremise ctx (Abs.TJudge prod label params t) scond) = do
    let rule = (Abs.TRule rname tpremise ctx (Abs.TJudge prod label params t) scond)
    checkRule <- genCheckRule rule
    inferRule <- genInferRule rule
    case t of
        Abs.TNone -> ErrM.Ok [checkRule]
        _ -> ErrM.Ok (checkRule : [inferRule])

getRuleName :: Abs.TypeRule -> String
getRuleName (Abs.TRule (Abs.RName (Abs.Ident x)) _ _ _ _) = x

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
            do_print [] = putStrLn ""
            do_print (x:xs) = do
                putStrLn x
                do_print xs