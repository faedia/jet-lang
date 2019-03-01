module JetCodeGen where

import qualified AbsJetGrammar as Abs;
import qualified ParJetGrammar as Par;
import ErrM (Err);
import qualified ErrM;
import JetIntermediateRepr;

tab = "    "
nltab = "\n" ++ tab

h2Str :: Abs.InlineHaskell -> String
h2Str (Abs.InlineHaskell s) = (tail . init) s

expandConstructor :: (Abs.Ident, [Abs.Ident]) -> String
expandConstructor (name, []) = id2Str name
expandConstructor (name, params) = "(" ++ expandConstructor (name, []) ++ foldr (\param s -> s ++ " " ++ id2Str param) "" params ++ ")"

genSingleMonad :: JetMonad -> String
genSingleMonad (JSideCond haskell) = h2Str haskell
genSingleMonad (JCheckFunc name ctx Abs.TNone var) = 
    "check" ++ id2Str name ++ " " ++ id2Str var ++ " " ++ h2Str ctx
genSingleMonad (JCheckFunc name ctx (Abs.TType tname tparams) var) =
    "check" ++ id2Str name ++ " " ++ id2Str var ++ " " ++ expandConstructor (tname, tparams) ++ " " ++ h2Str ctx

genMonadCode :: [JetMonad] -> String
genMonadCode = foldr (\m s -> genSingleMonad m ++ nltab ++ s) ""

genCheckCode :: JetCheck -> String
genCheckCode (JCheck astName astConst monads Abs.TNone ctx) = 
    "check" ++ id2Str astName ++ " " ++ expandConstructor astConst ++ " ctx = do" ++ nltab 
    ++ genMonadCode monads
    ++ "Ok (" ++ h2Str ctx ++ ")"
genCheckCode (JCheck astName astConst monads (Abs.TType typeName typeParams) ctx) = 
    "check" ++ id2Str astName ++ " " ++ expandConstructor astConst ++ " jetCheckType ctx = do" ++ nltab 
    ++ genMonadCode monads
    ++ "if jetCheckType == " ++ expandConstructor (typeName, typeParams) ++ " then Ok (" ++ h2Str ctx ++ ") else Bad \"Inconsistent Types\""

genInferCode :: JetInfer -> String
genInferCode JInferNone = ""
genInferCode (JInfer astName astConst monads (Abs.TType typeName typeParams)) = 
    "infer" ++ id2Str astName ++ " " ++ expandConstructor astConst ++ " ctx = do" ++ nltab 
    ++ genMonadCode monads
    ++ "Ok " ++ expandConstructor (typeName, typeParams)

genCode :: JetInterRepr -> String
genCode (JIntermediate haskell rules) = let checkRules = map fst rules; inferRules = map snd rules in 
    h2Str haskell ++ genCheckRulesCode checkRules ++ genInferRulesCode inferRules ++ "\n"
    where
        genCheckRulesCode :: [JetCheck] -> String
        genCheckRulesCode = foldr (\rule s -> s ++ "\n" ++ genCheckCode rule) ""
        genInferRulesCode :: [JetInfer] -> String
        genInferRulesCode = foldr (\rule s -> s ++ "\n" ++ genInferCode rule) ""
