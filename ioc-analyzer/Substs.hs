{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, FlexibleContexts #-}
module Substs
where

import Control.Applicative ((<$>))

import Data.Data
import Text.Parsec hiding (label, State, string, space)

import Parser

data Substs = Substs
    { s_file :: FilePath
    , s_vars :: [Ident]
    , s_values :: [[String]]
    }
  deriving (Show, Data, Typeable)

quotedVal = (concat <$> manyInQuotes rawAny) >>= ss
anyVal = choice [quotedVal, pvName]

substs = do
    exactWord "file"
    file <- quotedVal
    (vars, values) <- braces $ do
        exactWord "pattern"
        vars <- braces $ word `sepBy` comma
        values <- many $ braces $ anyVal `sepBy` comma
        return (vars, values)
    return $ Substs file vars values
