module JetErrorM where

import Control.Monad
import Control.Monad.Fail

data JetError a = Succ a | Fail String deriving (Show, Read, Eq, Ord)

instance Functor JetError where
    fmap = liftM

instance Applicative JetError where
    pure = Succ
    (<*>) = ap

instance Monad JetError where
    return = pure
    fail = Fail
    Succ a >>= f = f a
    Fail s >>= _ = Fail s

instance MonadFail JetError where
    fail = Fail

maybe2Error :: String -> Maybe a -> JetError a
maybe2Error s Nothing = Fail s
maybe2Error _ (Just a) = Succ a
