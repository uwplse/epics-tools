{
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Lexer (Token(..), PosToken, AlexPosn(..), alexScanTokens) where

import Data.Data

}

%wrapper "posn"

@int = [0-9\-][0-9]*
$wordchar = [a-zA-Z0-9_]
$wordstart = [a-zA-Z_]
@word = $wordstart $wordchar*
$space = $white # \n

tokens :-

\n              { out $ const EOL }
$space+         { out $ SPACE }
@word           { out $ WORD }
@int            { out $ INT }
.               { out $ SYM }

{

data Token =
    WORD String
  | INT String
  | SYM String
  | SPACE String
  | EOL
  deriving (Show, Data, Typeable)

type PosToken = (AlexPosn, Token)

deriving instance Typeable AlexPosn
deriving instance Data AlexPosn

out f p s = (p, f s)
}
