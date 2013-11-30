{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Language.TorqueScript.Quote (ts, torquescript) where

import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ)
import Text.Parsec (parse)
import Language.TorqueScript.Parser (file)

ts, torquescript :: QuasiQuoter

torquescript = QuasiQuoter {
    quoteExp = \s -> case parse file "" s of
        Left err -> fail $ show err
        Right s' -> dataToExpQ (const Nothing) s'

    , quotePat = undefined
    , quoteDec = undefined
    , quoteType = undefined
 }

ts = torquescript
