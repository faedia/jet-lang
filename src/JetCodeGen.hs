module JetCodeGen where

import qualified AbsJetGrammar as Abs;
import qualified ParJetGrammar as Par;
import ErrM (Err);
import qualified ErrM;
import JetIntermediateRepr;
import Data.List.Split
import Data.List

tab = "    " :: String
nltab = "\n" ++ tab :: String

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old
escapeChar :: String -> String -> String
escapeChar x = replace ("\\" ++ x) x
h2Str :: Abs.InlineHaskell -> String
h2Str (Abs.InlineHaskell s) = (escapeChar "{" . escapeChar "}" . tail . init) s

expandConstructor :: (Abs.Ident, [Abs.Ident]) -> String
expandConstructor (name, []) = id2Str name
expandConstructor (name, params) = "(" ++ expandConstructor (name, []) ++ " " ++ (init . foldr (\param s -> id2Str param ++ " " ++ s) "") params ++ ")"

wrapInferInCase :: JetMonad -> String
wrapInferInCase (JInferFunc name ctx var (Abs.TType tname tparams)) 
    | not (isTypeVar tname) = "case infer" ++ id2Str name ++ " " ++ id2Str var ++ " " ++ h2Str ctx ++ " of Succ jet0@" 
        ++ expandConstructor (tname, tparams) ++ " -> return jet0; Succ t -> fail (makeInferError " ++ id2Str var ++ " t); x -> x"
    | otherwise = "infer" ++ id2Str name ++ " " ++ id2Str var ++ " " ++ h2Str ctx
genSingleMonad :: JetMonad -> String
genSingleMonad (JSideCond haskell) = h2Str haskell
genSingleMonad (JCheckFunc count name ctx Abs.TNone var) = 
    "var" ++ show count ++ " <- check" ++ id2Str name ++ " " ++ id2Str var ++ " " ++ h2Str ctx
genSingleMonad (JCheckFunc count name ctx (Abs.TType tname tparams) var) =
    "var" ++ show count ++ " <- check" ++ id2Str name ++ " " ++ id2Str var ++ " " ++ expandConstructor (tname, tparams) ++ " " ++ h2Str ctx
genSingleMonad (JInferFunc name ctx var Abs.TNone) = 
    error "Infer functions require a type variable to return to"
genSingleMonad m@(JInferFunc name ctx var (Abs.TType tname tparams)) 
    | not (null (getTypeVars (Abs.TType tname tparams))) = 
        expandConstructor (tname, tparams) ++ " <- " ++ wrapInferInCase m
    | otherwise = error "Type must be a type variable to call infer"

genMonadCode :: [JetMonad] -> String
genMonadCode = foldr (\m s -> genSingleMonad m ++ nltab ++ s) ""

genTraceCode :: Abs.Ident -> (Abs.Ident, [Abs.Ident]) -> Abs.Type -> String -> String
genTraceCode astName astConst t funcName =
    "trace (\"" ++ funcName ++ " " ++ id2Str astName ++ " \" ++ show " ++ expandConstructor astConst ++ " ++ \" : " ++ tStr ++ "\") return ()"
    where
        tStr = case t of 
            Abs.TNone -> "Valid"
            Abs.TType tname tparams -> expandConstructor (tname, tparams)


genCheckCode :: Bool -> JetCheck -> String
genCheckCode tr (JCheck astName astConst monads Abs.TNone ctx) =
    "check" ++ id2Str astName ++ " " ++ expandConstructor astConst ++ " ctx = do" ++ nltab
    ++ (if tr then genTraceCode astName astConst Abs.TNone "check" ++ nltab else "") 
    ++ genMonadCode monads
    ++ h2Str ctx ++ "\n"
genCheckCode tr (JCheck astName astConst monads (Abs.TType typeName typeParams) ctx) = 
    "check" ++ id2Str astName ++ " " ++ expandConstructor astConst ++ " jetCheckType ctx = do" ++ nltab 
    ++ (if tr then genTraceCode astName astConst (Abs.TType typeName typeParams) "check" ++ nltab else "") 
    ++ genMonadCode monads
    ++ "if jetCheckType == " ++ expandConstructor (typeName, typeParams) ++ " then " ++ h2Str ctx ++ " else fail (makeCheckError " ++ expandConstructor astConst ++ " jetCheckType " ++ expandConstructor (typeName, typeParams) ++ ")\n"
genCheckCode tr (JCheckListEmpty astName monads Abs.TNone ctx) =
    "check" ++ id2Str astName ++ "List [] ctx = do" ++ nltab
    ++ (if tr then genTraceCode astName (Abs.Ident "", []) Abs.TNone "checkList" ++ nltab else "") 
    ++ genMonadCode monads
    ++ h2Str ctx ++ "\n"
genCheckCode tr (JCheckListSingleton astName item monads Abs.TNone ctx) =
    "check" ++ id2Str astName ++ "List [" ++ id2Str item ++ "] ctx = do" ++ nltab
    ++ (if tr then genTraceCode astName (item, []) Abs.TNone "checkList" ++ nltab else "") 
    ++ genMonadCode monads
    ++ h2Str ctx ++ "\n"
genCheckCode tr (JCheckListCons astName item list monads Abs.TNone ctx) =
    "check" ++ id2Str astName ++ "List (" ++ id2Str item ++ ":" ++ id2Str list ++ ") ctx = do" ++ nltab
    ++ (if tr then genTraceCode astName (item, [list]) Abs.TNone "checkList" ++ nltab else "") 
    ++ genMonadCode monads
    ++ h2Str ctx ++ "\n"
genCheckCode tr (JCheckListEmpty astName monads (Abs.TType typeName typeParams) ctx) =
    "check" ++ id2Str astName ++ "List [] jetCheckType ctx = do" ++ nltab
    ++ (if tr then genTraceCode astName (Abs.Ident "", []) Abs.TNone "checkList" ++ nltab else "") 
    ++ genMonadCode monads
    ++ "if jetCheckType == " ++ expandConstructor (typeName, typeParams) ++ " then " ++ h2Str ctx ++ " else fail (makeCheckErrorList [] jetCheckType " ++ expandConstructor (typeName, typeParams) ++ ")\n"
genCheckCode tr (JCheckListCons astName item list monads (Abs.TType typeName typeParams) ctx) =
    "check" ++ id2Str astName ++ "List (" ++ id2Str item ++ ":" ++ id2Str list ++ ") jetCheckType ctx = do" ++ nltab
    ++ (if tr then genTraceCode astName (item, [list]) Abs.TNone "checkList" ++ nltab else "") 
    ++ genMonadCode monads
    ++ "if jetCheckType == " ++ expandConstructor (typeName, typeParams) ++ " then " ++ h2Str ctx ++ " else fail (makeCheckErrorList (" ++ id2Str item ++ ":" ++ id2Str list ++ ") jetCheckType " ++ expandConstructor (typeName, typeParams) ++ ")\n"
    

genInferCode :: Bool -> JetInfer -> String
genInferCode tr JInferNone = ""
genInferCode tr (JInfer astName astConst monads (Abs.TType typeName typeParams)) = 
    "infer" ++ id2Str astName ++ " " ++ expandConstructor astConst ++ " ctx = do" ++ nltab 
    ++ (if tr then genTraceCode astName astConst (Abs.TType typeName typeParams) "infer" ++ nltab else "") 
    ++ genMonadCode monads
    ++ "return " ++ expandConstructor (typeName, typeParams) ++ "\n"
genInferCode tr (JInferListEmpty astName monads (Abs.TType typeName typeParams)) = 
    "infer" ++ id2Str astName ++ "List [] ctx = do" ++ nltab
    ++ (if tr then genTraceCode astName (Abs.Ident "", []) (Abs.TType typeName typeParams) "infer" ++ nltab else "") 
    ++ genMonadCode monads
    ++ "return " ++ expandConstructor (typeName, typeParams) ++ "\n"
genInferCode tr (JInferListCons astName item list monads (Abs.TType typeName typeParams)) = 
    "infer" ++ id2Str astName ++ "List ("++ id2Str item ++ ":" ++ id2Str list ++ ") ctx = do" ++ nltab
    ++ (if tr then genTraceCode astName (item, [list]) (Abs.TType typeName typeParams) "infer" ++ nltab else "") 
    ++ genMonadCode monads
    ++ "return " ++ expandConstructor (typeName, typeParams) ++ "\n"

genCode :: Bool -> JetInterRepr -> String
genCode tr (JIntermediate haskell rules) = let checkRules = map fst rules; inferRules = map snd rules in 
    h2Str haskell ++ "\n" ++ genCheckRulesCode checkRules ++ genInferRulesCode inferRules
    where
        genCheckRulesCode :: [JetCheck] -> String
        genCheckRulesCode = foldr (\rule s -> genCheckCode tr rule ++ s) ""
        genInferRulesCode :: [JetInfer] -> String
        genInferRulesCode = foldr (\rule s -> genInferCode tr rule ++ s) ""
