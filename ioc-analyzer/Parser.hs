{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, FlexibleContexts #-}
module Parser
where

import Control.Applicative ((<$>))
import Data.Data
import Data.Generics
import Data.Maybe
import Text.Parsec hiding (label, State, string, space)
import Text.Parsec.Pos (newPos)
import Numeric(readFloat)

import Lexer

import Debug.Trace


idtrace x = traceShow x x

showToken :: Token -> String
showToken (WORD w) = "word " ++ show w
showToken (INT i) = "int constant " ++ i
showToken (SYM s) = "symbol " ++ s
showToken (SPACE s) = "whitespace " ++ show s
showToken (EOL) = "end-of-line"

match pred = tokenPrim (showToken . snd) updatePos (testTok . snd)
  where
    testTok tt = if pred tt then Just tt else Nothing

updatePos oldPos (AlexPn _ line col, _) _ =
  flip setSourceLine   line $
  flip setSourceColumn col  $
  oldPos

rawWord' = match $ \t -> case t of WORD _ -> True; _ -> False
rawInt' = match $ \t -> case t of INT _ -> True; _ -> False
rawSym' = match $ \t -> case t of SYM _ -> True; _ -> False
rawSpace' = match $ \t -> case t of SPACE _ -> True; _ -> False
rawEol' = match $ \t -> case t of EOL -> True; _ -> False

rawWord = do WORD s <- rawWord'; return s
rawInt = do INT s <- rawInt'; return s
rawSym = do SYM s <- rawSym'; return s
rawSpace = do SPACE s <- rawSpace'; return s
rawEol = do EOL <- rawEol'; return "\n"

rawExactWord s = match $ \t -> case t of WORD s' | s == s' -> True; _ -> False
rawExactSym s = match $ \t -> case t of SYM s' | s == s' -> True; _ -> False


word = rawWord >>= ss
int = rawInt >>= ss
sym = rawSym >>= ss
optionalSpaces = many rawSpace

exactWord s = rawExactWord s >>= ss
exactSym s = rawExactSym s >>= ss

comma = exactSym ","
dollar = exactSym "$"

parens = between (exactSym "(") (exactSym ")")
brackets = between (exactSym "[") (exactSym "]")
braces = between (exactSym "{") (exactSym "}")
quotes = between (exactSym "\"") (exactSym "\"")
squotes = between (exactSym "\'") (exactSym "\'")


-- From the EPICS application developer's manual, valid characters in process
-- variable names are the following:
--      a-z A-Z 0-9 _ - : . [ ] < > ;
-- Strings consisting only of these characters do not need to be quoted.
rawPvChunk = tokenPrim (showToken . snd) updatePos (testTok . snd)
  where testTok tt = case tt of
            WORD s -> Just s
            INT i -> Just i
            SYM [c] | c `elem` "_-:.[]<>;" -> Just [c]
            _ -> Nothing
rawPvName = concat <$> many1 rawPvChunk

pvChunk = rawPvChunk >>= ss
pvName = rawPvName >>= ss



rawAny = choice [rawWord, rawInt, rawSym, rawSpace, rawEol]

comment = rawExactSym "#" >> many (choice [rawWord, rawInt, rawSym, rawSpace]) >> rawEol
ss x = many (choice [ rawSpace, rawEol, comment ]) >> return x


type Ident = String

data StringPart = Literal String | Subst Ident | SubstDefault Ident SubstString
  deriving (Eq, Ord, Show, Data, Typeable)
type SubstString = [StringPart]

substStringToStr :: SubstString -> String
substStringToStr [Literal l] = l

mergeParts (Literal a : Literal b : rest) = mergeParts $ Literal (a ++ b) : rest
mergeParts (p : ps) = p : mergeParts ps
mergeParts [] = []

manyInQuotes p =
    let quote = rawExactSym "\"" in
    let squote = rawExactSym "'" in
    (quote >> (p `manyTill` quote)) <|>
    (squote >> (p `manyTill` squote))

dbValue = substString <|> ((:[]) <$> varPart) <|> (return . Literal) <$> pvName

substString = manyInQuotes stringPart
stringPart = choice [varPart, Literal <$> rawAny]

varPart = do
    try $ rawExactSym "$" >> rawExactSym "("
    name <- word
    mVal <- optionMaybe $ rawExactSym "=" >> dbValue
    rawExactSym ")"

    case mVal of
        Just val -> return $ SubstDefault name val
        Nothing -> return $ Subst name


subVars f = everywhere (mkT $ (:[]) . Literal . go)
  where go :: SubstString -> String
        go = concatMap $ \part -> case part of
            Literal s -> s
            Subst v -> fromMaybe (error ("no value for " ++ v)) (f v)
            SubstDefault v d -> fromMaybe (go d) (f v)

maybeRead = fmap fst . listToMaybe . reads
maybeReadRational = fmap fst . listToMaybe . readFloat
