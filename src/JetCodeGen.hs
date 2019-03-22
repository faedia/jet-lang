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

genSingleMonad :: JetMonad -> String
genSingleMonad (JSideCond haskell) = h2Str haskell
genSingleMonad (JCheckFunc count name ctx Abs.TNone var) = 
    "var" ++ show count ++ " <- check" ++ id2Str name ++ " " ++ h2Str ctx  ++ " " ++ id2Str var
genSingleMonad (JCheckFunc count name ctx (Abs.TType tname tparams) var) =
    "var" ++ show count ++ " <- check" ++ id2Str name ++ " " ++ h2Str ctx  ++ " " ++ id2Str var ++ " " ++ expandConstructor (tname, tparams)
genSingleMonad (JInferFunc name ctx var Abs.TNone) = 
    error "Infer functions require a type variable to return to"
genSingleMonad (JInferFunc name ctx var (Abs.TType tname tparams)) 
    | isTypeVar (Abs.TType tname tparams) = 
        id2Str tname ++ " <- infer" ++ id2Str name ++ " " ++ h2Str ctx  ++ " " ++ id2Str var
    | otherwise = error "Type must be a type variable to call infer"

genMonadCode :: [JetMonad] -> String
genMonadCode = foldr (\m s -> genSingleMonad m ++ nltab ++ s) ""

genCheckCode :: JetCheck -> String
genCheckCode (JCheck astName astConst monads Abs.TNone ctx) = 
    "check" ++ id2Str astName ++ " ctx " ++ expandConstructor astConst ++ " = do" ++ nltab 
    ++ genMonadCode monads
    ++ h2Str ctx ++ "\n"
genCheckCode (JCheck astName astConst monads (Abs.TType typeName typeParams) ctx) = 
    "check" ++ id2Str astName ++ " ctx " ++ expandConstructor astConst ++ " jetCheckType = do" ++ nltab 
    ++ genMonadCode monads
    ++ "if jetCheckType == " ++ expandConstructor (typeName, typeParams) ++ " then " ++ h2Str ctx ++ " else Bad (\"Expected type \" ++ show jetCheckType ++ \" found type \" ++ show " ++ expandConstructor (typeName, typeParams) ++ ")\n"
genCheckCode (JCheckListEmpty astName monads Abs.TNone ctx) =
    "check" ++ id2Str astName ++ "List ctx [] = do" ++ nltab
    ++ genMonadCode monads
    ++ h2Str ctx ++ "\n"
genCheckCode (JCheckListSingleton astName item monads Abs.TNone ctx) =
    "check" ++ id2Str astName ++ "List ctx [" ++ id2Str item ++ "] = do" ++ nltab
    ++ genMonadCode monads
    ++ h2Str ctx ++ "\n"
genCheckCode (JCheckListCons astName item list monads Abs.TNone ctx) =
    "check" ++ id2Str astName ++ "List ctx (" ++ id2Str item ++ ":" ++ id2Str list ++ ") = do" ++ nltab
    ++ genMonadCode monads
    ++ h2Str ctx ++ "\n"
genCheckCode (JCheckListEmpty astName monads (Abs.TType typeName typeParams) ctx) =
    "check" ++ id2Str astName ++ "List ctx [] jetCheckType = do" ++ nltab
    ++ genMonadCode monads
    ++ "if jetCheckType == " ++ expandConstructor (typeName, typeParams) ++ " then " ++ h2Str ctx ++ " else Bad (\"Expected type \" ++ show jetCheckType ++ \" found type \" ++ show " ++ expandConstructor (typeName, typeParams) ++ ")\n"
genCheckCode (JCheckListCons astName item list monads (Abs.TType typeName typeParams) ctx) =
    "check" ++ id2Str astName ++ "List ctx (" ++ id2Str item ++ ":" ++ id2Str list ++ ") jetCheckType= do" ++ nltab
    ++ genMonadCode monads
    ++ "if jetCheckType == " ++ expandConstructor (typeName, typeParams) ++ " then " ++ h2Str ctx ++ " else Bad (\"Expected type \" ++ show jetCheckType ++ \" found type \" ++ show " ++ expandConstructor (typeName, typeParams) ++ ")\n"
    

genInferCode :: JetInfer -> String
genInferCode JInferNone = ""
genInferCode (JInfer astName astConst monads (Abs.TType typeName typeParams)) = 
    "infer" ++ id2Str astName ++ " ctx " ++ expandConstructor astConst ++ " = do" ++ nltab 
    ++ genMonadCode monads
    ++ "Ok " ++ expandConstructor (typeName, typeParams) ++ "\n"
genInferCode (JInferListEmpty astName monads (Abs.TType typeName typeParams)) = 
    "infer" ++ id2Str astName ++ "List ctx [] = do" ++ nltab
    ++ genMonadCode monads
    ++ "Ok " ++ expandConstructor (typeName, typeParams) ++ "\n"
genInferCode (JInferListCons astName item list monads (Abs.TType typeName typeParams)) = 
    "infer" ++ id2Str astName ++ "List ctx ("++ id2Str item ++ ":" ++ id2Str list ++ ") = do" ++ nltab
    ++ genMonadCode monads
    ++ "Ok " ++ expandConstructor (typeName, typeParams) ++ "\n"

genCode :: JetInterRepr -> String
genCode (JIntermediate haskell rules) = let checkRules = map fst rules; inferRules = map snd rules in 
    h2Str haskell ++ "\n" ++ genCheckRulesCode checkRules ++ genInferRulesCode inferRules
    where
        genCheckRulesCode :: [JetCheck] -> String
        genCheckRulesCode = foldr (\rule s -> genCheckCode rule ++ s) ""
        genInferRulesCode :: [JetInfer] -> String
        genInferRulesCode = foldr (\rule s -> genInferCode rule ++ s) ""
