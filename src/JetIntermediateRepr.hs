module JetIntermediateRepr where

import qualified AbsJetGrammar as Abs;
import Data.Char (isLower)
import Data.Set (Set)
import qualified Data.Set as S

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

isTypeVar :: Abs.Ident -> Bool
isTypeVar = isLower . head . id2Str

getTypeVars :: Abs.Type -> Set Abs.Ident
getTypeVars (Abs.TType t []) = if isTypeVar t then S.singleton t else S.empty
getTypeVars (Abs.TType t ts) = if isTypeVar t then error "typevar cannot take parameters" else
    foldr (\t' ts' -> if isTypeVar t' then S.insert t' ts' else ts') S.empty ts


genIntermediateRepr :: Abs.TypeSystem -> JetInterRepr
genIntermediateRepr (Abs.TSystem s typerules) = JIntermediate s (genTypeRules typerules)

genTypeRules :: [Abs.TypeRule] -> [JetRuleRepr]
genTypeRules = map genTypeRule

genPremis :: Int -> Set Abs.Ident -> Abs.JudgementWSC -> JetMonad
genPremis _ _ (Abs.JSideCond haskell) = JSideCond haskell
genPremis count inferedTypeVars (Abs.JJudge (Abs.JSingle ctx name var [] t)) = let tvars = getTypeVars t in
    if not (null tvars) then
        if tvars `S.isSubsetOf` inferedTypeVars then
            JCheckFunc count name ctx t var
        else
            JInferFunc name ctx var t
    else
        JCheckFunc count name ctx t var
genPremis count inferedTypeVars (Abs.JJudge (Abs.JList ctx name vars t)) 
    | length vars == 1 = let funcname = idCat name "List"; var = head vars; tvars = getTypeVars t in
        if not (null tvars) then
            if tvars `S.isSubsetOf` inferedTypeVars then
                JCheckFunc count funcname ctx t var
            else
                JInferFunc funcname ctx var t
        else
            JCheckFunc count funcname ctx t var
    | otherwise = error "Only one variable supported as parameter to list decl" 

genPremises :: Int -> Set Abs.Ident -> Abs.TypePremises -> [JetMonad]
genPremises _ _ Abs.TPNone = []
genPremises _ inferedTypeVars (Abs.TPremis []) = []
genPremises count inferedTypeVars (Abs.TPremis (j:js)) = 
    let premis = genPremis count inferedTypeVars j in
        case premis of
            JInferFunc _ _ _ t -> premis : genPremises count (getTypeVars t `S.union` inferedTypeVars) (Abs.TPremis js)
            JCheckFunc {} -> premis : genPremises (count + 1) inferedTypeVars (Abs.TPremis js)
            _ -> premis : genPremises count inferedTypeVars (Abs.TPremis js)

genTypeRule :: Abs.TypeRule -> JetRuleRepr
genTypeRule (Abs.TRule name premises (Abs.JSingle ctx astType astConst astConstParams Abs.TNone)) = 
    (JCheck astType (astConst, astConstParams) (genPremises 1 S.empty premises) Abs.TNone ctx, JInferNone)
genTypeRule (Abs.TRule name premises (Abs.JSingle ctx astType astConst astConstParams t)) = 
    (
        JCheck astType (astConst, astConstParams) (genPremises 1 S.empty premises) t ctx,
        JInfer astType (astConst, astConstParams) (genPremises 1 S.empty premises) t
    )
genTypeRule (Abs.TRule name premises (Abs.JList ctx astType vars Abs.TNone))
    | null vars = 
        (JCheckListEmpty astType (genPremises 1 S.empty premises) Abs.TNone ctx, JInferNone)
    | length vars == 1 = 
        (JCheckListSingleton astType (head vars) (genPremises 1 S.empty premises) Abs.TNone ctx, JInferNone)
    | length vars == 2 = 
        (JCheckListCons astType (head vars) (vars !! 1) (genPremises 1 S.empty premises) Abs.TNone ctx, JInferNone)
    | otherwise = error "Invalid number of arguments to list constructor"
genTypeRule (Abs.TRule name premises (Abs.JList ctx astType vars t))
    | null vars = 
        (
            JCheckListEmpty astType (genPremises 1 S.empty premises) t ctx,
            JInferListEmpty astType (genPremises 1 S.empty premises) t
        )
    | length vars == 1 = error "Singleton not supported"
    | length vars == 2 = 
        (
            JCheckListCons astType (head vars) (vars !! 1) (genPremises 1 S.empty premises) t ctx,
            JInferListCons astType (head vars) (vars !! 1) (genPremises 1 S.empty premises) t
        )
    