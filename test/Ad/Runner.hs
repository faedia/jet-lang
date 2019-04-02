module Runner where

import ErrM
import TypAd
import ParAd
import JetContext
import JetErrorM

compiler s = case pProgram (myLexer s) of
    Bad err -> err
    Ok tree -> case checkProgram tree (emptyContext :: Context) of 
        Fail err -> err
        Succ _ -> "Type Good"
