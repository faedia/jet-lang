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

tab = "    "
nltab = "\n" ++ tab

expandDef :: Abs.Ident -> [Abs.Ident] -> String
expandDef var params = id2Str var ++ foldr (\a b -> b ++ " " ++ id2Str a) "" params

genFuncPattern :: String -> Abs.Ident -> String
genFuncPattern str id = str ++ id2Str id ++ " ctx "

genCheckFuncDefn :: Abs.TypeJudgement -> String
genCheckFuncDefn (Abs.TJudge prod label vars tSign) = genFuncPattern "check" prod ++ typevar ++ " (" ++ expandDef label vars ++ ") = do" ++ nltab
    where    
    typevar = case tSign of
        Abs.TNone -> ""
        _ -> "jetCheckType"

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
                nltab ++ sideCond scond 
            else 
                "")

genPremisesCheck :: [Abs.TypePremises] -> Set Abs.Ident -> String
genPremisesCheck [] typeSet = ""
genPremisesCheck (Abs.TPremises prod nodeId typeNode :ps) typeSet = case typeNode of
    Abs.TNone -> ctxMonad ++ dotab ++ genPremisesCheck ps typeSet
    Abs.TType tvar tparams -> if isTypeVar tvar then
            if tvar `Set.member` typeSet then
                ctxMonad ++ " " ++ id2Str tvar ++ dotab ++ genPremisesCheck ps typeSet
            else
                typeVarMonad tvar ++ dotab ++ genPremisesCheck ps (Set.insert tvar typeSet)
        else
            ctxMonad ++ " (" ++ expandDef tvar tparams ++ ") " ++ dotab ++ genPremisesCheck ps typeSet
    where
        dotab = if null ps then "" else nltab
        genMonad str func = str ++ " <- " ++ genFuncPattern func prod ++ id2Str nodeId
        ctxMonad = genMonad "ctx" "check"
        typeVarMonad tvar = genMonad (id2Str tvar) "infer"

genCheckRule :: Abs.TypeRule -> ErrM.Err String
genCheckRule (Abs.TRule name premises ctx judgement scond) = case premises of
    Abs.TNoPremis -> case judgement of
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars Abs.TNone -> if null vars then
                ErrM.Ok (genCheckFuncDefn judgement ++ "Ok (" ++ ctxDefnCode ctx ++ ")")
            else
                ErrM.Bad "If the current node has no children then we should have no premises"
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars (Abs.TType tvar tparams) ->
            let t' = getT tvar tparams scond in
                if isTypeVar tvar then
                    if null tparams then
                        ErrM.Ok (genCheckFuncDefn judgement ++ t' ++ nltab ++ genIfExpr (id2Str tvar))
                    else
                        ErrM.Bad "Type Vars don't take params"
                else
                    ErrM.Ok (genCheckFuncDefn judgement ++ genIfExpr (expandDef tvar tparams))
    Abs.TPremisCond ps -> case judgement of 
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars Abs.TNone -> 
            ErrM.Ok (genCheckFuncDefn judgement ++  genPremisesCheck ps Set.empty ++ nltab ++ "Ok (" ++ ctxDefnCode ctx ++ ")")
        Abs.TJudge (Abs.Ident prodName) (Abs.Ident labelName) vars (Abs.TType tvar tparams) ->
            ErrM.Ok (genCheckFuncDefn judgement ++  genPremisesCheck ps Set.empty ++ nltab ++ genIfExpr (expandDef tvar tparams))
    where
        genIfExpr :: String -> String
        genIfExpr str = "if jetCheckType == (" ++ str ++ ") then Ok (" ++ ctxDefnCode ctx ++ ") else Bad \"Inconsistent types\""

genInferRule :: Abs.TypeRule -> ErrM.Err String
genInferRule rule = ErrM.Ok "-- TODO: Infer"

generateFromRule :: Abs.TypeRule -> ErrM.Err [String]
generateFromRule (Abs.TRule rname tpremise ctx (Abs.TJudge prod label params t) scond) = do
    let rule = (Abs.TRule rname tpremise ctx (Abs.TJudge prod label params t) scond)
    checkRule <- genCheckRule rule
    inferRule <- genInferRule rule
    case t of
        Abs.TNone -> ErrM.Ok [checkRule ++ "\n"]
        _ -> ErrM.Ok ((checkRule ++ "\n") : [inferRule ++ "\n"])

getAllRules :: Abs.TypeSystem -> [Abs.TypeRule]
getAllRules (Abs.TSystem rules) = rules

gen :: [Abs.TypeRule] -> ErrM.Err [String]
gen [] = ErrM.Ok []
gen (r:rs) = do
    rgen <- generateFromRule r
    rsgen <- gen rs
    ErrM.Ok (rgen ++ rsgen)

run :: String ->  ErrM.Err [String]
run s = do
    tree <- (Par.pTypeSystem . Par.myLexer) s
    let rules = getAllRules tree
    gen rules

main :: IO ()
main = do
    contents <- readFile "test.txt"
    let rules = run contents
    case rules of
        ErrM.Bad err -> print err
        ErrM.Ok rs -> putStr (concat rs)