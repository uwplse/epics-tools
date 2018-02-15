{
{-# LANGUAGE FlexibleContexts #-}

module ExpParser (BinOp(..), UnaryOp(..), Exp(..), parseExp) where
import Text.Parsec
import Text.Parsec.Pos (newPos)
import Control.Monad (mfilter)
import Data.Char (toUpper)
}

%wrapper "posn"

@num = [0-9][0-9\.]*
$wordchar = [a-zA-Z0-9_]
$wordstart = [a-zA-Z_]
@word = $wordstart $wordchar*
@paren = [\(\)\[\]\{\}]
@op = [\|\&\=\>\<]+ | (\:\=) | (\!\=) | [\!\-\+\*\;\,\?\:\#\%\/\$\~]
@strchar1 = \\. | [^\\\']
@strchar2 = \\. | [^\\\"]
@str1 = '@strchar1*'
@str2 = "@strchar2*"

tokens :-

$white+         ;
@op             { out $ OpTok }
@word           { out $ NameTok }
@num            { out $ (NumTok . read) }
@paren          { out $ (ParenTok . head) }
@str1           { out $ \s -> StrTok $ read $ "\"" ++ (dropFirstAndLast s) ++ "\"" }
@str2           { out $ (StrTok . read) }

{

dropFirstAndLast a = drop 1 (take (length a - 1) a)

data ExpToken =
    NameTok String
  | NumTok Double
  | StrTok String
  | OpTok String
  | ParenTok Char
  deriving (Show)

out f p s = (p, f s)

data BinOp = Sequence | Plus | Times | Minus | Div | Mod | Pow | Eq | Ne | Gt | Ge | Lt | Le | And | Or | BitAnd | BitOr | BitXor | ShiftLeft | ShiftRight
  deriving (Eq, Show)

data UnaryOp = Not | Neg | BitNot
  deriving (Eq, Show)

data Exp =
  EName String |
  EVal Double |
  EStr String |
  ECall String [Exp] |
  EUnaryOp UnaryOp Exp |
  EBinOp BinOp Exp Exp |
  ECond Exp Exp Exp |
  EAssign String Exp |
  ESeq Exp Exp |
  ESubarrayInPlace Exp Exp Exp | -- e{start,end}
  ESubarray Exp Exp Exp          -- e[start,end]
  deriving (Eq, Show)

-- I would *REALLY* like to use Text.Parsec.Expr for this, but it doesn't
-- easily support the ternary operator. >:(

match pred = token (show . snd) (convertPos . fst) (mfilter pred . Just . snd)
convertPos (AlexPn _ line col) = newPos "<input>" line col

isOpTok o t = case t of { OpTok x | x == o -> True; _ -> False }
isNameTok t = case t of { NameTok _ -> True; _ -> False }
isNumTok t = case t of { NumTok _ -> True; _ -> False }
isStrTok t = case t of { StrTok _ -> True; _ -> False }
isParenTok c t = case t of { ParenTok x | x == c -> True; _ -> False }

op o = match (isOpTok o) >> return o

-- from https://wiki-ext.aps.anl.gov/epics/index.php/RRM_3-14_Calculation:
-- "All alphabetic elements described below are case independent, so upper and
-- lower case letters may be used and mixed in the variable and function names
-- as desired."
-- So, let's just uppercase everything...
name = do (NameTok n) <- match isNameTok; return (map toUpper n)

number = do (NumTok n) <- match isNumTok; return n
str = do (StrTok s) <- match isStrTok; return s
openParen = match (isParenTok '(') >> return ()
closeParen = match (isParenTok ')') >> return ()
openBracket = match (isParenTok '[') >> return ()
closeBracket = match (isParenTok ']') >> return ()
openBrace = match (isParenTok '{') >> return ()
closeBrace = match (isParenTok '}') >> return ()
callSpecial = do
  -- from http://www.aps.anl.gov/bcda/synApps/calc/R2-4/sCalcoutRecord.html:
  -- $P is a synonym for PRINTF, $S is a synonym for SSCANF
  op "$"
  n <- name
  es <- between openParen closeParen (sepBy expr (op ","))
  case n of
    "P" -> return $ ECall "PRINTF" es
    "S" -> return $ ECall "SSCANF" es
    _ -> error $ "failed to parse $" ++ n
nameOrCall = do
  n <- name
  choice [
    do
      es <- between openParen closeParen (sepBy expr (op ","))
      return $ ECall n es,
    do
      return $ EName n]
term = between openParen closeParen expr <|> callSpecial <|> nameOrCall <|> (number >>= (return . EVal)) <|> (str >>= (return . EStr))

unary next = choice [
  op "!" >> unary next >>= return . (EUnaryOp Not),
  op "~" >> unary next >>= return . (EUnaryOp BitNot),
  op "-" >> unary next >>= return . (EUnaryOp Neg),
  next]

subarray next = do
  e <- next
  choice [
    between openBracket closeBracket $ do
      start <- expr
      op ","
      end <- expr
      return $ ESubarray e start end,
    between openBrace closeBrace $ do
      start <- expr
      op ","
      end <- expr
      return $ ESubarrayInPlace e start end,
    do
      return e]

binary s f next = chainl1 next (s >> return f)

ternary next = do
  e1 <- next
  choice [
    do
      op "?"
      e2 <- ternary next
      op ":"
      e3 <- ternary next
      return $ ECond e1 e2 e3,
    do
      return e1]

assignment next = do
  e1 <- next
  case e1 of
    EName n -> choice [
      do
        op ":="
        e2 <- assignment next
        return $ EAssign n e2,
      do
        return e1]
    _ -> return e1

levels = [
  -- TODO: figure out the exact precedence & associativity rules for EPICS
  subarray,
  binary (op "*") (EBinOp Times), binary (op "/") (EBinOp Div), binary (op "%") (EBinOp Mod),
  binary (op "+") (EBinOp Plus), binary (op "-") (EBinOp Minus),
  binary (op "|") (EBinOp BitOr), binary (op "&") (EBinOp BitAnd),
  binary (op ">=") (EBinOp Ge), binary (op ">") (EBinOp Gt), binary (op "<") (EBinOp Lt), binary (op "<=") (EBinOp Le),
  binary (op "==" <|> op "=") (EBinOp Eq), binary (op "!=" <|> op "#") (EBinOp Ne),
  binary (op "||") (EBinOp Or), binary (op "&&") (EBinOp And),
  binary (op "<<") (EBinOp ShiftLeft), binary (op ">>") (EBinOp ShiftRight),
  ternary,
  assignment,
  binary (op ";") ESeq
  ]

-- From https://wiki-ext.aps.anl.gov/epics/index.php/RRM_3-14_Calculation:
-- "The string may contain a series of expressions separated by a semi-colon
-- character ';' any one of which may actually provide the calculation result;
-- however all of the other expressions included must assign their result to a
-- variable."
expr = foldr ($) (unary term) (reverse levels)

parseExp :: String -> Either ParseError Exp
parseExp input = parse (do e <- expr; eof; return e) input (alexScanTokens input)

}
