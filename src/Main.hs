module Main where

import qualified AbsJetGrammar as Abs;
import qualified ParJetGrammar as Par;
import qualified ErrM;
import Data.List;
import Data.Char;
import Data.Set (Set);
import qualified Data.Set as Set;

isTypeVar :: Abs.Ident -> Bool
isTypeVar (Abs.Ident str) = isLower (head str)

getInlineHaskell :: Abs.InlineHaskell -> String
getInlineHaskell (Abs.InlineHaskell str) = (tail . init) str

ctxDefnCode :: Abs.ContextDefn -> String
ctxDefnCode (Abs.CtxDefn code) = getInlineHaskell code

id2Str :: Abs.Ident -> String
id2Str (Abs.Ident str) = str

genCheckFuncDefn :: Abs.TypeJudgement -> String
genCheckFuncDefn (Abs.TJudge prod label vars tSign) = "check" ++ id2Str prod ++ " ctx " ++ typevar ++ " (" ++ id2Str label ++ foldr (\a b -> b ++ " " ++ id2Str a) "" vars ++ ") = "
    where    
    typevar = case tSign of
        Abs.TNone -> ""
        _ -> "jetTypeCheck"

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
        "let t = " ++ id2Str tvar ++ foldr (\a b -> b ++ " " ++ id2Str a) "" tparams ++ (
            if hasSideCond scond then 
                "\n\t" ++ sideCond scond 
            else 
                "")

expandTypeDef :: Abs.Type -> String
expandTypeDef (Abs.TType tvar tparams) = id2Str tvar ++ foldr (\a b -> b ++ " " ++ id2Str a) "" tparams

genPremisesCheck :: [Abs.TypePremises] -> Set Abs.Ident -> String
genPremisesCheck [] typeSet = ""
genPremisesCheck (Abs.TPremises prod nodeId typeNode :ps) typeSet = case typeNode of
    Abs.TNone -> "\n\tctx <- check" ++ id2Str prod ++ " ctx " ++ id2Str nodeId ++ "\n\t" ++ genPremisesCheck ps typeSet
    Abs.TType tvar tparams -> if isTypeVar tvar then
            if tvar `Set.member` typeSet then
                "\n\tctx <- check" ++ id2Str prod ++ " ctx " ++ id2Str nodeId ++ " " ++ id2Str tvar ++ "" ++ genPremisesCheck ps typeSet
            else
                "\n\t" ++ id2Str tvar ++ " <- infer" ++ id2Str prod ++ " ctx " ++ id2Str nodeId ++ "" ++ genPremisesCheck ps (Set.insert tvar typeSet)
        else
            "\n\tctx <- check" ++ id2Str prod ++ "ctx " ++ id2Str nodeId ++ " (" ++ expandTypeDef typeNode ++ ") " ++ genPremisesCheck ps typeSet

genCheckRule :: Abs.TypeRule -> ErrM.Err String
genCheckRule (Abs.TRule name premises ctx judgement scond) = case premises of
    Abs.TNoPremis -> case judgement of
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars Abs.TNone -> if null vars then
                ErrM.Ok (genCheckFuncDefn judgement ++ " = Ok " ++ ctxDefnCode ctx)
            else
                ErrM.Bad "If the current node has no children then we should have no premises"
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars (Abs.TType tvar tparams) ->
            let t' = getT tvar tparams scond in
                if isTypeVar tvar then
                    if null tparams then
                        ErrM.Ok (genCheckFuncDefn judgement ++ "do\n\t" ++ t' ++ "\n\tif jetCheckType == " ++ id2Str tvar ++ " then Ok (" ++ ctxDefnCode ctx ++ ") else Bad \"Inconsistent types\"")
                    else
                        ErrM.Bad "Type Vars don't take params"
                else
                    ErrM.Ok (genCheckFuncDefn judgement ++ "do\n\tif jetCheckType == ("++ expandTypeDef (Abs.TType tvar tparams)  ++ ") then Ok (" ++ ctxDefnCode ctx ++ ") else Bad \"Inconsistent types\"")
    Abs.TPremisCond ps -> case judgement of 
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars Abs.TNone -> 
            ErrM.Ok (genCheckFuncDefn judgement ++ "do" ++ genPremisesCheck ps Set.empty ++ "\n\tOk (" ++ ctxDefnCode ctx ++ ")")
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars (Abs.TType tvar tparams) ->
            ErrM.Ok (genCheckFuncDefn judgement ++ "do" ++ genPremisesCheck ps Set.empty ++ "\n\t" ++ "if jetCheckType == (" ++ expandTypeDef (Abs.TType tvar tparams) ++ ") then Ok (" ++ ctxDefnCode ctx ++ ") else Bad \"Inconsistent types\"")

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