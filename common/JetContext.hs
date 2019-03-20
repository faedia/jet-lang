{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module JetContext where

import Data.Map (Map)
import qualified Data.Map as M

newtype JetContextMap k t = JetContextMap [Map k t]

insertUnique :: Ord k => k -> a -> Map k a -> Maybe (Map k a)
insertUnique k v m = case M.lookup k m of
    Just _ -> fail "Unable to insert duplicate entries"
    Nothing -> return (M.insert k v m)

class JetContextBase t where
    emptyContext :: t
    newBlock :: t -> t

class JetContextBase r => JetContext r k t where
    lookupBlock :: Ord k => k -> r -> Maybe t
    lookupContext :: Ord k => k -> r -> Maybe t
    expandContext :: Ord k => k -> t -> r -> Maybe r
    expandContextIf :: Ord k => (k -> r -> Bool) -> k -> t -> r -> Maybe r

instance JetContextBase (JetContextMap a b) where
    emptyContext = JetContextMap [M.empty]
    newBlock (JetContextMap blocks) = JetContextMap (M.empty : blocks)

instance JetContext (JetContextMap a b) a b where
    lookupBlock key (JetContextMap (block : _)) = M.lookup key block
    lookupContext key (JetContextMap blocks) = M.lookup key (M.unions blocks)
    expandContext key t (JetContextMap (block:blocks)) = do
        block' <- insertUnique key t block
        return (JetContextMap (block':blocks))
    expandContextIf predicate key t ctx = 
        if predicate key ctx then expandContext key t ctx
        else fail "Predicate check failed"