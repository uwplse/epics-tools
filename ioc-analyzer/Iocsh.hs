{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, FlexibleContexts #-}
module Iocsh
where

import Control.Applicative ((<$>))

import Data.Data
import Text.Parsec hiding (label, State, string, space)

import Parser


data Command = FuncCmd Ident [String] | OtherCmd [String] | NoCmd
  deriving (Show, Data, Typeable)

quotedVal = (concat <$> manyInQuotes rawAny) >>= ss
anyVal = choice [quotedVal, pvName]

funcCmd = do
    name <- word
    args <- parens $ anyVal `sepBy` comma
    return $ FuncCmd name args

otherCmd = do
    parts <- concat <$> rawAny `manyTill` rawEol
    many rawEol
    return $ OtherCmd [parts]

commandComment = do
  exactSym "#"
  rawAny `manyTill` rawEol
  return $ NoCmd

command = optionalSpaces >> (commandComment <|> try funcCmd <|> otherCmd)


loadRecordSubsts =
    (do k <- rawWord; rawExactSym "="; v <- rawWord; return (k,v)) `sepBy` rawExactSym ","
