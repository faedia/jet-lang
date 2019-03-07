module JetIntermediateRepr where

import qualified AbsJetGrammar as Abs;
import Data.Char (isLower)

data JetMonad = 
    JSideCond Abs.InlineHaskell | 
    JCheckFunc Int Abs.Ident Abs.InlineHaskell Abs.Type Abs.Ident |
    JInferFunc Abs.Ident Abs.InlineHaskell Abs.Ident Abs.Type
    deriving (Eq, Ord, Show, Read)

data JetCheck = JCheck Abs.Ident (Abs.Ident, [Abs.Ident]) [JetMonad] Abs.Type Abs.InlineHaskell |
    JCheckListEmpty Abs.Ident [JetMonad] Abs.Type Abs.InlineHaskell |
    JCheckListSingleton Abs.Ident Abs.Ident [JetMonad] Abs.Type Abs.InlineHaskell |
    JCheckListCons Abs.Ident Abs.Ident Abs.Ident [JetMonad] Abs.Type Abs.InlineHaskell
    deriving (Eq, Ord, Show, Read)
data JetInfer = JInfer Abs.Ident (Abs.Ident, [Abs.Ident]) [JetMonad] Abs.Type | 
    JInferListEmpty Abs.Ident [JetMonad] Abs.Type |
    JInferListSingleton Abs.Ident Abs.Ident [JetMonad] Abs.Type |
    JInferListCons Abs.Ident Abs.Ident Abs.Ident [JetMonad] Abs.Type |
    JInferNone
    deriving (Eq, Ord, Show, Read)

type JetRuleRepr = (JetCheck, JetInfer)

data JetInterRepr = JIntermediate Abs.InlineHaskell [JetRuleRepr]
    deriving (Eq, Ord, Show, Read)

idCat :: Abs.Ident -> String -> Abs.Ident
idCat (Abs.Ident ident) s = Abs.Ident (ident ++ s) 

id2Str :: Abs.Ident -> String
id2Str (Abs.Ident s) = s

isTypeVar :: Abs.Type -> Bool
isTypeVar Abs.TNone = False
isTypeVar (Abs.TType t []) = (isLower . head . id2Str) t

genIntermediateRepr :: Abs.TypeSystem -> JetInterRepr
genIntermediateRepr (Abs.TSystem s typerules) = JIntermediate s (genTypeRules typerules)

genTypeRules :: [Abs.TypeRule] -> [JetRuleRepr]
genTypeRules = map genTypeRule

genPremis :: Int -> [Abs.Type] -> Abs.JudgementWSC -> JetMonad
genPremis _ _ (Abs.JSideCond haskell) = JSideCond haskell
genPremis count inferedTypeVars (Abs.JJudge (Abs.JSingle ctx name var [] t)) = 
    if isTypeVar t then
        if t `elem` inferedTypeVars then
            JCheckFunc count name ctx t var
        else
            JInferFunc name ctx var t
    else
        JCheckFunc count name ctx t var
genPremis count inferedTypeVars (Abs.JJudge (Abs.JList ctx name vars t)) 
    | length vars == 1 = let funcname = idCat name "List"; var = head vars in
        if isTypeVar t then
            if t `elem` inferedTypeVars then
                JCheckFunc count funcname ctx t var
            else
                JInferFunc funcname ctx var t
        else
            JCheckFunc count funcname ctx t var
    | otherwise = error "Only one variable supported as parameter to list decl" 

genPremises :: Int -> [Abs.Type] -> Abs.TypePremises -> [JetMonad]
genPremises _ _ Abs.TPNone = []
genPremises _ inferedTypeVars (Abs.TPremis []) = []
genPremises count inferedTypeVars (Abs.TPremis (j:js)) = 
    let premis = genPremis count inferedTypeVars j in
        case premis of
            JInferFunc _ _ _ t -> premis : genPremises count (t:inferedTypeVars) (Abs.TPremis js)
            JCheckFunc {} -> premis : genPremises (count + 1) inferedTypeVars (Abs.TPremis js)
            _ -> premis : genPremises count inferedTypeVars (Abs.TPremis js)

genTypeRule :: Abs.TypeRule -> JetRuleRepr
genTypeRule (Abs.TRule name premises (Abs.JSingle ctx astType astConst astConstParams Abs.TNone)) = 
    (JCheck astType (astConst, astConstParams) (genPremises 1 [] premises) Abs.TNone ctx, JInferNone)
genTypeRule (Abs.TRule name premises (Abs.JSingle ctx astType astConst astConstParams t)) = 
    (
        JCheck astType (astConst, astConstParams) (genPremises 1 [] premises) t ctx,
        JInfer astType (astConst, astConstParams) (genPremises 1 [] premises) t
    )
genTypeRule (Abs.TRule name premises (Abs.JList ctx astType vars Abs.TNone))
    | null vars = 
        (JCheckListEmpty astType (genPremises 1 [] premises) Abs.TNone ctx, JInferNone)
    | length vars == 1 = 
        (JCheckListSingleton astType (head vars) (genPremises 1 [] premises) Abs.TNone ctx, JInferNone)
    | length vars == 2 = 
        (JCheckListCons astType (head vars) (vars !! 1) (genPremises 1 [] premises) Abs.TNone ctx, JInferNone)
    | otherwise = error "Invalid number of arguments to list constructor"
genTypeRule (Abs.TRule name premises (Abs.JList ctx astType vars t))
    | null vars = 
        (
            JCheckListEmpty astType (genPremises 1 [] premises) t ctx,
            JInferListEmpty astType (genPremises 1 [] premises) t
        )
    | length vars == 1 = error "Singleton not supported"
    | length vars == 2 = 
        (
            JCheckListCons astType (head vars) (vars !! 1) (genPremises 1 [] premises) t ctx,
            JInferListCons astType (head vars) (vars !! 1) (genPremises 1 [] premises) t
        )
    