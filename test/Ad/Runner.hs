module Runner where

import ErrM
import TypAd
import ParAd
import Context

compiler s = case pProgram (myLexer s) of
    Bad err -> err
    Ok tree -> case checkProgram tree emptyCtx of 
        Bad err -> err
        Ok _ -> "Type Good"
