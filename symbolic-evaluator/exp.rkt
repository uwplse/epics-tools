#lang rosette

(require 2htdp/abstraction)
(provide (all-defined-out))

; This module defines types and functions for handling EPICS calc expressions.

; The language defined here is very low-level, with separate operations for each
; type. We rely on the Haskell generator to write out the appropriate ops.

(define-type Exp

  ; Read a value from a record's field
  (ELoad (record string?) (field string?))

  ; Constant values
  (EBoolConst           (val boolean?))
  (EBvConst8            (val (bitvector 8)))
  (EBvConst16           (val (bitvector 16)))
  (EBvConst32           (val (bitvector 32)))
  (EBvConst64           (val (bitvector 64)))
  (EFloatConst          (val (bitvector 32))) ; TODO: proper float handling
  (EDoubleConst         (val (bitvector 64))) ; TODO: proper double handling

  ; Boolean operators
  (EBoolEq              (e1 Exp?) (e2 Exp?))
  (EBoolAnd             (e1 Exp?) (e2 Exp?))
  (EBoolOr              (e1 Exp?) (e2 Exp?))
  (EBoolNot             (e Exp?))

  ; Bit-vector operators
  (EBvPlus              (e1 Exp?) (e2 Exp?))
  (EBvTimes             (e1 Exp?) (e2 Exp?))
  (EBvMinus             (e1 Exp?) (e2 Exp?))
  (EBvDiv               (e1 Exp?) (e2 Exp?))
  (EBvMod               (e1 Exp?) (e2 Exp?))
  (EBvPow               (e1 Exp?) (e2 Exp?))
  (EBvEq                (e1 Exp?) (e2 Exp?))
  (EBvNe                (e1 Exp?) (e2 Exp?))
  (EBvGt                (e1 Exp?) (e2 Exp?))
  (EBvGe                (e1 Exp?) (e2 Exp?))
  (EBvLt                (e1 Exp?) (e2 Exp?))
  (EBvLe                (e1 Exp?) (e2 Exp?))
  (EBvBitAnd            (e1 Exp?) (e2 Exp?))
  (EBvBitOr             (e1 Exp?) (e2 Exp?))
  (EBvBitXor            (e1 Exp?) (e2 Exp?))
  (EBvShiftLeft         (e1 Exp?) (e2 Exp?))
  (EBvAShiftRight       (e1 Exp?) (e2 Exp?))
  (EBvLShiftRight       (e1 Exp?) (e2 Exp?))
  (EBvNeg               (e Exp?))
  (EBvNot               (e Exp?))

  ; Vector operators
  (EVecPlus             (e1 Exp?) (e2 Exp?))
  (EVecEq               (e1 Exp?) (e2 Exp?))

  ; Float operators
  (EFloatPlus           (e1 Exp?) (e2 Exp?))
  (EFloatTimes          (e1 Exp?) (e2 Exp?))
  (EFloatMinus          (e1 Exp?) (e2 Exp?))
  (EFloatDiv            (e1 Exp?) (e2 Exp?))
  (EFloatMod            (e1 Exp?) (e2 Exp?))
  (EFloatPow            (e1 Exp?) (e2 Exp?))
  (EFloatEq             (e1 Exp?) (e2 Exp?))
  (EFloatNe             (e1 Exp?) (e2 Exp?))
  (EFloatGt             (e1 Exp?) (e2 Exp?))
  (EFloatGe             (e1 Exp?) (e2 Exp?))
  (EFloatLt             (e1 Exp?) (e2 Exp?))
  (EFloatLe             (e1 Exp?) (e2 Exp?))

  ; Double operators
  (EDoublePlus          (e1 Exp?) (e2 Exp?))
  (EDoubleTimes         (e1 Exp?) (e2 Exp?))
  (EDoubleMinus         (e1 Exp?) (e2 Exp?))
  (EDoubleDiv           (e1 Exp?) (e2 Exp?))
  (EDoubleMod           (e1 Exp?) (e2 Exp?))
  (EDoublePow           (e1 Exp?) (e2 Exp?))
  (EDoubleEq            (e1 Exp?) (e2 Exp?))
  (EDoubleNe            (e1 Exp?) (e2 Exp?))
  (EDoubleGt            (e1 Exp?) (e2 Exp?))
  (EDoubleGe            (e1 Exp?) (e2 Exp?))
  (EDoubleLt            (e1 Exp?) (e2 Exp?))
  (EDoubleLe            (e1 Exp?) (e2 Exp?))

  ; Casts
  (EFloat->Int32        (e Exp?))
  (EInt32->Float        (e Exp?))
  (EDouble->Int64       (e Exp?))
  (EInt64->Double       (e Exp?))
  (EBvSignExtend        (e Exp?) (old-width number?) (new-width number?))
  (EBvZeroExtend        (e Exp?) (old-width number?) (new-width number?))
  (EBvShorten           (e Exp?) (old-width number?) (new-width number?))

  )


(define-type Op
  (StoreOp (record string?) (field string?) (val Exp?))
  (SeqOp (op1 Op?) (op2 Op?)))
