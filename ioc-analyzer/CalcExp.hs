{-# LANGUAGE DeriveDataTypeable #-}
module CalcExp where

import Data.Generics


data Exp =
      Var Int       -- double var (A .. L)
    | XVar Int      -- extended var (AA .. LL)
    | Lit Double    -- double literal
    | LitStr String
    | Unary UnOp Exp
    | Binary BinOp Exp Exp
    | Ternary TernOp Exp Exp Exp
    | Varary VarOp [Exp]
    | Assign Int Exp
    | XAssign Int Exp
    | Seq Exp Exp
  deriving (Eq, Show, Data, Typeable)

dummyExp = Lit 0

data UnOp =
      Abs | Sqr | Ceil | Floor
    | Log | Ln | Exp
    | NotAlg
    | Sin | SinH | ASin
    | Cos | CosH | ACos
    | Tan | TanH | ATan
    | NotLog
    | NotBit
    | ArrRep
  deriving (Eq, Show, Data, Typeable)

data BinOp =
      Pow   -- TODO: ^ and ** are the same thing, right?
    | Add | Sub | Mul | Div | Mod
    | Ge | Gt | Le | Lt | Ne | Eq
    | AndLog | OrLog
    | OrBit | AndBit | XorBit
    | ShiftL | ShiftR
  deriving (Eq, Show, Data, Typeable)

data TernOp =
      Cond
    | Slice
    | SliceInPlace
  deriving (Eq, Show, Data, Typeable)

data VarOp =
      Min | Max
    | Finite | IsNan
    | Printf
  deriving (Eq, Show, Data, Typeable)
