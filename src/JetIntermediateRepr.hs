module JetIntermediateRepr where

import qualified AbsJetGrammar as Abs;
import Data.Char (isLower)

data JetMonad = 
    JSideCond Abs.InlineHaskell | 
    JCheckFunc Abs.Ident Abs.InlineHaskell Abs.Type Abs.Ident |
    JInferFunc Abs.Ident Abs.InlineHaskell Abs.Ident Abs.Type
    deriving (Eq, Ord, Show, Read)

data JetCheck = JCheck Abs.Ident (Abs.Ident, [Abs.Ident]) [JetMonad] Abs.Type Abs.InlineHaskell
    deriving (Eq, Ord, Show, Read)
data JetInfer = JInfer Abs.Ident (Abs.Ident, [Abs.Ident]) [JetMonad] Abs.Type | JInferNone
    deriving (Eq, Ord, Show, Read)

type JetRuleRepr = (JetCheck, JetInfer)

data JetInterRepr = JIntermediate Abs.InlineHaskell [JetRuleRepr]
    deriving (Eq, Ord, Show, Read)

id2Str :: Abs.Ident -> String
id2Str (Abs.Ident s) = s

isTypeVar :: Abs.Type -> Bool
isTypeVar Abs.TNone = False
isTypeVar (Abs.TType t []) = (isLower . head . id2Str) t

genIntermediateRepr :: Abs.TypeSystem -> JetInterRepr
genIntermediateRepr (Abs.TSystem s typerules) = JIntermediate s (genTypeRules typerules)

genTypeRules :: [Abs.TypeRule] -> [JetRuleRepr]
genTypeRules = map genTypeRule

genPremis :: [Abs.Type] -> Abs.JudgementWSC -> JetMonad
genPremis _ (Abs.JSideCond haskell) = JSideCond haskell
genPremis inferedTypeVars (Abs.JJudge (Abs.JSingle ctx name var [] t)) = 
    if isTypeVar t then
        if t `elem` inferedTypeVars then
            JCheckFunc name ctx t var
        else
            JInferFunc name ctx var t
    else
        JCheckFunc name ctx t var

genPremises :: [Abs.Type] -> Abs.TypePremises -> [JetMonad]
genPremises _ Abs.TPNone = []
genPremises inferedTypeVars (Abs.TPremis []) = []
genPremises inferedTypeVars (Abs.TPremis (j:js)) = 
    let premis = genPremis inferedTypeVars j in
        case premis of
            JInferFunc _ _ _ t -> premis : genPremises (t:inferedTypeVars) (Abs.TPremis js)
            _ -> premis : genPremises inferedTypeVars (Abs.TPremis js)

genTypeRule :: Abs.TypeRule -> JetRuleRepr
genTypeRule (Abs.TRule name premises (Abs.JSingle ctx astType astConst astConstParams Abs.TNone)) = 
    (JCheck astType (astConst, astConstParams) (genPremises [] premises) Abs.TNone ctx, JInferNone)
genTypeRule (Abs.TRule name premises (Abs.JSingle ctx astType astConst astConstParams t)) = 
    (
        JCheck astType (astConst, astConstParams) (genPremises [] premises) t ctx,
        JInfer astType (astConst, astConstParams) (genPremises [] premises) t
    )