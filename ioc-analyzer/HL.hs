{-# LANGUAGE TemplateHaskell, DataKinds, Rank2Types, ScopedTypeVariables,
        FlexibleContexts #-}
module HL
where

import Control.Applicative ((<$>), Alternative(..))
import Control.Monad
import Control.Monad.State
import Control.Lens hiding ((^.), use)
import Data.Either (lefts, rights)
import Data.Maybe (fromJust, fromMaybe)
import Data.Int
import qualified Data.Map as M
import qualified Data.Set as S

import ExpParser as E
import Multi
import Parser
import Record
import FieldInfo

import qualified CalcExp as CE

import Debug.Trace


-- WTF.  Control.Lens.(^.) seems to be completely broken - it demands a Monoid
-- instance for no apparent reason.  So here I've defined a version that
-- actually works...

data MyConst r a = No | Yes r

instance Functor (MyConst r) where
    fmap _ No = No
    fmap _ (Yes x) = Yes x

instance Applicative (MyConst r) where
    pure _ = No
    (Yes x) <*> _ = Yes x
    _ <*> (Yes x) = Yes x
    _ <*> _ = No

unConst No = error $ "unConst: No"
unConst (Yes x) = x

x ^. l = unConst $ l Yes x
infixl 8 ^.
use l = gets $ \x -> x ^. l


data MenuScan =
      Scan_Passive
    | Scan_Periodic Double
    | Scan_IOIntr
    deriving (Eq, Show)

data MenuAlarmSevr =
      AlarmSevr_NO_ALARM
    | AlarmSevr_MINOR
    | AlarmSevr_MAJOR
    | AlarmSevr_INVALID
    deriving (Eq, Show)

data MenuOOpt =
      OOpt_EveryTime
    | OOpt_OnChange
    | OOpt_WhenZero
    | OOpt_WhenNonzero
    | OOpt_TransitionToZero
    | OOpt_TransitionToNonzero
    deriving (Eq, Show)

data MenuDOpt =
      DOpt_UseCALC
    | DOpt_UseOCAL
    deriving (Eq, Show)

data MenuOmsl =
      Omsl_Supervisory
    | Omsl_ClosedLoop
    deriving (Eq, Show)

data MenuSelM =
      SelM_All
    | SelM_Specified
    deriving (Eq, Show)

data MenuFtype =
      Ftype_String
    | Ftype_Char
    | Ftype_UChar
    | Ftype_Short
    | Ftype_UShort
    | Ftype_Long
    | Ftype_ULong
    | Ftype_Float
    | Ftype_Double
    | Ftype_Enum
    deriving (Eq, Show)

data MenuConv =
      Conv_None
    | Conv_Slope
    | Conv_Linear
    deriving (Eq, Show)

data HlAlarm = HlAlarm
    { _alarm_HIHI :: Double
    , _alarm_HIGH :: Double
    , _alarm_LOW :: Double
    , _alarm_LOLO :: Double
    , _alarm_HHSV :: MenuAlarmSevr
    , _alarm_HSV :: MenuAlarmSevr
    , _alarm_LSV :: MenuAlarmSevr
    , _alarm_LLSV :: MenuAlarmSevr
    }
  deriving (Show)
makeLenses ''HlAlarm

data HlRecordType =
      RtCalc
    | RtCalcOut
    | RtStrCalcOut
    | RtArrayCalcOut
    | RtAnalogOut
    | RtAnalogIn
    | RtBinaryIn
    | RtBinaryOut
    | RtMBBO
    | RtStringIn
    | RtStringOut
    | RtLongIn
    | RtLongOut
    | RtWaveform
    | RtSubArray
    | RtFanout
    | RtDFanout
    | RtSeq
    | RtAsyn
    | RtHavoc
  deriving (Eq, Ord, Show)

-- TODO: replace this with an enum
type DeviceType = String

defaultDtyp = "Soft Channel"

data HlDetail =
      Calc
      { _calc_CALC :: CE.Exp
      , _calc_INPA_to_INPL :: Multi 12 FieldLink
      , _calc_A_to_L :: Multi 12 Double
      , _calc_VAL :: Double
      }
    | CalcOut
      { _calcout_CALC :: CE.Exp
      , _calcout_OCAL :: CE.Exp
      , _calcout_INPA_to_INPL :: Multi 12 FieldLink
      , _calcout_OUT :: FieldLink
      , _calcout_OOPT :: MenuOOpt
      , _calcout_DOPT :: MenuDOpt
      , _calcout_A_to_L :: Multi 12 Double
      , _calcout_VAL :: Double
      , _calcout_OVAL :: Double
      }
    | StrCalcOut
      { _scalcout_CALC :: CE.Exp
      , _scalcout_OCAL :: CE.Exp
      , _scalcout_INPA_to_INPL :: Multi 12 FieldLink
      , _scalcout_INAA_to_INLL :: Multi 12 FieldLink
      , _scalcout_OUT :: FieldLink
      , _scalcout_OOPT :: MenuOOpt
      , _scalcout_DOPT :: MenuDOpt
      , _scalcout_A_to_L :: Multi 12 Double
      , _scalcout_AA_to_LL :: Multi 12 String
      , _scalcout_VAL :: Double
      , _scalcout_SVAL :: String
      , _scalcout_OVAL :: Double
      , _scalcout_OSV :: String
      }
    | ArrayCalcOut
      { _acalcout_NELM :: Int
      , _acalcout_CALC :: CE.Exp
      , _acalcout_OCAL :: CE.Exp
      , _acalcout_INPA_to_INPL :: Multi 12 FieldLink
      , _acalcout_INAA_to_INLL :: Multi 12 FieldLink
      , _acalcout_OUT :: FieldLink
      , _acalcout_OOPT :: MenuOOpt
      , _acalcout_DOPT :: MenuDOpt
      , _acalcout_A_to_L :: Multi 12 Double
      , _acalcout_AA_to_LL :: Multi 12 [Double]
      , _acalcout_VAL :: Double
      , _acalcout_AVAL :: [Double]
      , _acalcout_OVAL :: Double
      , _acalcout_OAV :: [Double]
      }
    | AnalogOut
      { _ao_DOL :: FieldLink
      , _ao_VAL :: Double
      , _ao_OMSL :: MenuOmsl
      , _ao_DTYP :: DeviceType
      , _ao_LINR :: MenuConv
      , _ao_ESLO :: Double
      , _ao_EOFF :: Double
      , _ao_alarm :: HlAlarm
      }
    | AnalogIn
      { _ai_INP :: FieldLink
      , _ai_VAL :: Double
      , _ai_DTYP :: DeviceType
      , _ai_LINR :: MenuConv
      , _ai_ESLO :: Double
      , _ai_EOFF :: Double
      , _ai_alarm :: HlAlarm
      }
    | BinaryIn
      { _bi_INP :: FieldLink
      , _bi_VAL :: Int      -- actually limited to 0..1
      , _bi_ZNAM_ONAM :: Multi 2 String
      , _bi_DTYP :: DeviceType
      , _bi_ZSV_OSV :: Multi 2 MenuAlarmSevr
      }
    | BinaryOut
      { _bo_DOL :: FieldLink
      , _bo_VAL :: Int    -- actually limited to 0..1
      , _bo_ZNAM_ONAM :: Multi 2 String
      , _bo_OMSL :: MenuOmsl
      , _bo_DTYP :: DeviceType
      , _bo_ZSV_OSV :: Multi 2 MenuAlarmSevr
      }
    | MBBO
      { _mbbo_DOL :: FieldLink
      , _mbbo_VAL :: Int    -- actually limited to 0..16
      , _mbbo_ZRST_to_FFST :: Multi 16 String
      , _mbbo_OMSL :: MenuOmsl
      , _mbbo_ZRSV_to_FFSV :: Multi 16 MenuAlarmSevr
      , _mbbo_DTYP :: DeviceType
      }
    | StringIn
      { _stringin_INP :: FieldLink
      , _stringin_VAL :: String
      , _stringin_alarm :: HlAlarm
      , _stringin_DTYP :: DeviceType
      }
    | StringOut
      { _stringout_DOL :: FieldLink
      , _stringout_VAL :: String
      , _stringout_OMSL :: MenuOmsl
      , _stringout_DTYP :: DeviceType
      }
    | LongIn
      { _longin_INP :: FieldLink
      , _longin_VAL :: Int32
      , _longin_alarm :: HlAlarm
      , _longin_DTYP :: DeviceType
      }
    | LongOut
      { _longout_DOL :: FieldLink
      , _longout_VAL :: Int32
      , _longout_OMSL :: MenuOmsl
      , _longout_DTYP :: DeviceType
      }
    | Waveform
      { _waveform_DTYP :: DeviceType
      , _waveform_INP :: FieldLink
      , _waveform_FTVL :: MenuFtype
      , _waveform_NELM :: Int
      }
    | SubArray
      { _subarray_INP :: FieldLink
      , _subarray_FTVL :: MenuFtype
      , _subarray_MALM :: Int
      , _subarray_NELM :: Int
      , _subarray_INDX :: Int
      }
    | Fanout
      { _fanout_LNK1_to_LNK6 :: Multi 6 RecordLink
      }
    | DFanout
      { _dfanout_DOL :: FieldLink
      , _dfanout_OUTA_to_OUTH :: Multi 8 FieldLink
      , _dfanout_OMSL :: MenuOmsl
      , _dfanout_VAL :: Double
      }
    | Seq
      { _seq_SELM :: MenuSelM
      , _seq_SELL :: FieldLink
      , _seq_DOL1_to_DOLA :: Multi 10 FieldLink
      , _seq_DO1_to_DOA :: Multi 10 Double
      , _seq_LNK1_to_LNKA :: Multi 10 FieldLink
      , _seq_DLY1_to_DLYA :: Multi 10 Double
      }
    | Asyn {}
    | Havoc
      { _havoc_in_links :: [FieldLink]
      , _havoc_out_links :: [FieldLink]
      , _havoc_fwd_links :: [RecordLink]
      }
  deriving (Show)

makeLenses ''HlDetail

data HlRecord = HlRecord
    { _hlr_NAME :: String
    , _hlr_FLNK :: RecordLink
    , _hlr_PINI :: Bool
    , _hlr_SCAN :: MenuScan
    , _hlr_ASG :: String
    , _hlr_SDIS :: FieldLink
    , _hlr_DISA :: Int  -- actually limited to 0..2^16
    , _hlr_DISV :: Int  -- actually limited to 0..2^16
    , _hlr_aliases :: [String]
    , _detail :: HlDetail
    }
  deriving (Show)
makeLenses ''HlRecord

type HlDatabase = M.Map String HlRecord


data Result a = Ok a | Err String | TryNext

instance Functor Result where
    fmap f (Ok x) = Ok (f x)
    fmap _ (Err e) = Err e
    fmap _ TryNext = TryNext

instance Applicative Result where
    pure x = Ok x
    Ok f <*> Ok x = Ok (f x)
    Err e <*> _ = Err e
    TryNext <*> _ = TryNext

instance Monad Result where
    Ok x >>= k = k x
    Err e >>= _ = Err e
    TryNext >>= _ = TryNext
    fail e = Err e

instance Alternative Result where
    empty = TryNext
    TryNext <|> x = x
    x <|> _ = x

instance MonadPlus Result where

type M a = Result a

ctx s k = case k of
    Ok x -> Ok x
    Err e -> Err (e ++ "\n...while " ++ s)
    TryNext -> TryNext


overM :: Monad m => (forall f. Applicative f => (a -> f b) -> (s -> f t)) -> (a -> m b) -> s -> m t
overM l f x = l f x

setM :: Monad m => (forall f. Applicative f => (a -> f b) -> (s -> f t)) -> m b -> s -> m t
setM l k x = overM l (const k) x

idx :: forall a m f. (MultiOps a m, Functor f) => Int -> (a -> f a) -> (m -> f m)
idx i = lens (\m -> multiGet m i) (\m x -> multiSet m i x)


parseRecord :: Record -> M HlRecord
parseRecord r =
    case ty_name of
        "calc" -> go init_calc update_calc r
        "ao" -> go init_ao update_ao r
        "ai" -> go init_ai update_ai r
        "calcout" -> go init_calcout update_calcout r
        "scalcout" -> go init_scalcout update_scalcout r
        "acalcout" -> go init_acalcout update_acalcout r
        "bi" -> go init_bi update_bi r
        "bo" -> go init_bo update_bo r
        "mbbo" -> go init_mbbo update_mbbo r
        "stringin" -> go init_stringin update_stringin r
        "stringout" -> go init_stringout update_stringout r
        "longin" -> go init_longin update_longin r
        "longout" -> go init_longout update_longout r
        "waveform" -> go init_waveform update_waveform r
        "subArray" -> go init_subarray update_subarray r
        "fanout" -> go init_fanout update_fanout r
        "dfanout" -> go init_dfanout update_dfanout r
        "asyn" -> go init_asyn update_asyn r
        "seq" -> go init_seq update_seq r
        ty -> Err $ "unrecognized record type: " ++ show ty
  where
    rn = substStringToStr $ r_name r
    ty_name = substStringToStr $ r_type r

    go :: HlDetail -> (String -> String -> HlDetail -> M HlDetail)
            -> Record -> M HlRecord
    go init update r = ctx ("parsing " ++ ty_name ++ " record " {- ++ show rn -}) $
        let name = substStringToStr $ r_name r
            aliases = map substStringToStr $ r_aliases r
        in foldM (\rc (fn, fv_) ->
            let fv = substStringToStr fv_ in
            ctx ("parsing field " ++ show fn ++ " = " ++ show fv) $ msum $
                [ updateCommon fn fv rc
                , overM detail (update fn fv) rc
                , Err $ "unrecognized " ++ ty_name ++ " field: " ++ show fn
                ]
        ) (initCommon name aliases init) (r_fields r)

parseRecordFallback :: Record -> M HlRecord
parseRecordFallback r = go init_havoc (update_havoc ty_name) r
  where
    rn = substStringToStr $ r_name r
    ty_name = substStringToStr $ r_type r

    go :: HlDetail -> (String -> String -> HlDetail -> M HlDetail)
            -> Record -> M HlRecord
    go init update r = ctx ("parsing " ++ ty_name ++ " record " ++ show rn ++ " (fallback)") $
        let name = substStringToStr $ r_name r
            aliases = map substStringToStr $ r_aliases r
        in foldM (\rc (fn, fv_) ->
            let fv = substStringToStr fv_ in
            ctx ("parsing field " ++ show fn ++ " = " ++ show fv) $ msum $
                [ updateCommon fn fv rc
                , overM detail (update fn fv) rc
                , Err $ "unrecognized " ++ ty_name ++ " field: " ++ show fn
                ]
        ) (initCommon name aliases init) (r_fields r)

parseRecordWithFallback :: Record -> M HlRecord
parseRecordWithFallback r =
    case parseRecord r of
        Err _ -> parseRecordFallback r
        x -> x

resultToEither :: Result a -> Either String a
resultToEither (Ok x) = Right x
resultToEither (Err e) = Left e
resultToEither TryNext = Left "TryNext"

parseDatabase :: Database -> (HlDatabase, [String])
parseDatabase db =
    let ms = map (resultToEither . parseRecord) db
        errs = lefts ms
        hlrs = rights ms
        hldb = M.fromList [(hlr ^. hlr_NAME, hlr) | hlr <- hlrs]
    in (hldb, errs)

parseDatabaseWithFallback :: Database -> (HlDatabase, [String])
parseDatabaseWithFallback db =
    let ms = map (resultToEither . parseRecordWithFallback) db
        errs = lefts ms
        hlrs = rights ms
        hldb = M.fromList [(hlr ^. hlr_NAME, hlr) | hlr <- hlrs]
    in (hldb, errs)


readExp s = case parseExp s of
    Left err -> Err $ "error parsing " ++ show s ++ ": " ++ show err
    Right exp -> Ok exp


flattenESeq (ESeq e1 e2) = flattenESeq e1 ++ flattenESeq e2
flattenESeq (EBinOp Sequence e1 e2) = flattenESeq e1 ++ flattenESeq e2
flattenESeq e = [e]

isEAssign (EAssign _ _) = True
isEAssign _ = False

readCalcExp (EName name) = case name of
    [c] | Just i <- letterToIndex 12 c -> return $ CE.Var i
    [c,c'] | c == c', Just i <- letterToIndex 12 c -> return $ CE.XVar i
    _ -> Err $ "unrecognized name in expression: " ++ name
readCalcExp (EVal x) = return $ CE.Lit x
readCalcExp (EStr x) = return $ CE.LitStr x
readCalcExp (ECall f es) = do
    es' <- mapM readCalcExp es
    case f of
        "ABS" -> mkUnary CE.Abs es'
        "SQR" -> mkUnary CE.Sqr es'
        "MIN" -> return $ CE.Varary CE.Min es'
        "MAX" -> return $ CE.Varary CE.Max es'
        "FINITE" -> return $ CE.Varary CE.Finite es'
        "ISNAN" -> return $ CE.Varary CE.IsNan es'
        "CEIL" -> mkUnary CE.Ceil es'
        "FLOOR" -> mkUnary CE.Floor es'
        "LOG" -> mkUnary CE.Log es'
        "LOGE" -> mkUnary CE.Ln es'
        "LN" -> mkUnary CE.Ln es'
        "EXP" -> mkUnary CE.Exp es'
        "SIN" -> mkUnary CE.Sin es'
        "SINH" -> mkUnary CE.SinH es'
        "ASIN" -> mkUnary CE.ASin es'
        "COS" -> mkUnary CE.Cos es'
        "COSH" -> mkUnary CE.CosH es'
        "ACOS" -> mkUnary CE.ACos es'
        "TAN" -> mkUnary CE.Tan es'
        "TANH" -> mkUnary CE.TanH es'
        "ATAN" -> mkUnary CE.ATan es'
        "PRINTF" -> return $ CE.Varary CE.Printf es'
        "ARR" -> mkUnary CE.ArrRep es'
        -- TODO: OR, AND, XOR seem to be functions, probably varary?
        _ -> Err $ "unrecognized function name: " ++ show f
  where
        mkUnary op [e] = return $ CE.Unary op e
        mkUnary op es = Err $
            "expected exactly one argument to " ++ show op ++ ", but got " ++ show (length es)
readCalcExp (EUnaryOp op e) = do
    e' <- readCalcExp e
    op' <- case op of
        Not -> return CE.NotLog
        Neg -> return CE.NotAlg
        BitNot -> return CE.NotBit
        -- _ -> Err $ "unsupported operator: " ++ show op
    return $ CE.Unary op' e'
readCalcExp (EBinOp op e1 e2) = do
    e1' <- readCalcExp e1
    e2' <- readCalcExp e2
    op' <- case op of
        Plus -> return CE.Add
        Times -> return CE.Mul
        Minus -> return CE.Sub
        Div -> return CE.Div
        Mod -> return CE.Mod
        Pow -> return CE.Pow
        Ge -> return CE.Ge
        Gt -> return CE.Gt
        Le -> return CE.Le
        Lt -> return CE.Lt
        Ne -> return CE.Ne
        Eq -> return CE.Eq
        And -> return CE.AndLog
        Or -> return CE.OrLog
        BitAnd -> return CE.AndBit
        BitOr -> return CE.OrBit
        BitXor -> return CE.XorBit
        ShiftLeft -> return CE.ShiftL
        ShiftRight -> return CE.ShiftR
        _ -> Err $ "unsupported operator: " ++ show op
    return $ CE.Binary op' e1' e2'
readCalcExp (ECond e1 e2 e3) = do
    e1' <- readCalcExp e1
    e2' <- readCalcExp e2
    e3' <- readCalcExp e3
    return $ CE.Ternary CE.Cond e1' e2' e3'
readCalcExp (EAssign name e) = do
    e' <- readCalcExp e
    case name of
        [c] | Just i <- letterToIndex 12 c -> return $ CE.Assign i e'
        [c,c'] | c == c', Just i <- letterToIndex 12 c -> return $ CE.XAssign i e'
        _ -> Err $ "unrecognized name in expression: " ++ name
readCalcExp (ESeq e1 e2) = do
    e1' <- readCalcExp e1
    e2' <- readCalcExp e2
    return $ CE.Seq e1' e2'
readCalcExp (ESubarray arr e1 e2) = do
    arr' <- readCalcExp arr
    e1' <- readCalcExp e1
    e2' <- readCalcExp e2
    return $ CE.Ternary CE.Slice arr' e1' e2'
readCalcExp (ESubarrayInPlace arr e1 e2) = do
    arr' <- readCalcExp arr
    e1' <- readCalcExp e1
    e2' <- readCalcExp e2
    return $ CE.Ternary CE.SliceInPlace arr' e1' e2'
--readCalcExp e = Err $ "unsupported exp in calc: " ++ show e


data LinkFlag =
      PpLinkFlag PpFlag
    | MsLinkFlag MsFlag

readLinkFlag :: String -> M LinkFlag
readLinkFlag s = case s of
    "NPP" -> Ok $ PpLinkFlag NoProcessPassive
    "PP" -> Ok $ PpLinkFlag ProcessPassive
    "NMS" -> Ok $ MsLinkFlag NonMaximizeSeverity
    "MS" -> Ok $ MsLinkFlag MaximizeSeverity
    "MSS" -> Ok $ MsLinkFlag MaximizeStatusAndSeverity
    "MSI" -> Ok $ MsLinkFlag MaximizeSeverityIfInvalid
    _ -> Err $ "unrecognized link flag: " ++ show s

calcPpFlag :: [LinkFlag] -> M PpFlag
calcPpFlag fs = fromMaybe NoProcessPassive <$>
    foldM (\cur lf -> case (lf, cur) of
        (PpLinkFlag pp, Nothing) -> return $ Just pp
        (PpLinkFlag _, Just _) -> Err $ "duplicate process-passive flags for link"
        _ -> return cur
    ) Nothing fs

calcMsFlag :: [LinkFlag] -> M MsFlag
-- TODO: check that NMS is actually the default
calcMsFlag fs = fromMaybe NonMaximizeSeverity <$>
    foldM (\cur lf -> case (lf, cur) of
        (MsLinkFlag ms, Nothing) -> return $ Just ms
        (MsLinkFlag _, Just _) -> Err $ "duplicate maximize-severity flags for link"
        _ -> return cur
    ) Nothing fs

readFieldLink :: String -> M FieldLink
readFieldLink "" = return NoLink
readFieldLink ('@' : addr) = return $ HardwareAddr addr
readFieldLink s | Just n <- maybeRead s = return $ Constant n
readFieldLink s = ctx ("parsing link " ++ show s) $ do
    (target : flags) <- return $ words s
    let (rn, fn) = case break (== '.') target of
            (rn, []) -> (rn, "VAL")
            (rn, fn) -> (rn, tail fn)
    linkFlags <- mapM readLinkFlag flags
    pp <- calcPpFlag linkFlags
    ms <- calcMsFlag linkFlags
    return $ FieldLink rn fn pp ms

readRecordLink :: String -> M RecordLink
readRecordLink "" = return NoRLink
readRecordLink s = ctx ("parsing link " ++ show s) $ do
    (target : flags) <- return $ words s
    rn <- case break (== '.') target of
        (rn, []) -> return rn
        (rn, '.' : fn)
            | fn == "PROC" -> return rn
            | otherwise -> Err $ "unexpected field in record link: " ++ show fn
    when (not $ null flags) $ Err $ "unexpected flags in record link: " ++ show flags
    -- TODO: check if it refers to an actual record
    return $ RecordLink rn

readDouble :: String -> M Double
readDouble s | Just n <- maybeRead s = return n
readDouble s = Err $ "expected double, but got: " ++ show s

readInt :: String -> M Int
readInt s | Just n <- maybeRead s = return n
readInt s = Err $ "expected integer, but got: " ++ show s

readInt32 :: String -> M Int32
readInt32 s | Just n <- maybeRead s = return n
readInt32 s = Err $ "expected Int32, but got: " ++ show s

readDeviceType :: String -> M DeviceType
readDeviceType s = return s


-- Convert A..Z to a zero-based index
letterToIndex max c = do
    let ix = fromEnum c - fromEnum 'A'
    guard $ 0 <= ix && ix < max
    return ix

-- Convert 1..9 to a zero-based index
digitToIndex max c = do
    let ix = fromEnum c - fromEnum '1'
    guard $ 0 <= ix && ix < max
    return ix

-- Convert 1..9 + A..Z to a zero-based index
charToIndex m c | m > 9, Just i <- letterToIndex (m - 9) c = Just (9 + i)
charToIndex m c = digitToIndex (min m 9) c

codeToIndex "ZR" = Just 0
codeToIndex "ON" = Just 1
codeToIndex "TW" = Just 2
codeToIndex "TH" = Just 3
codeToIndex "FR" = Just 4
codeToIndex "FV" = Just 5
codeToIndex "SX" = Just 6
codeToIndex "SV" = Just 7
codeToIndex "EI" = Just 8
codeToIndex "NI" = Just 9
codeToIndex "TE" = Just 10
codeToIndex "EL" = Just 11
codeToIndex "TV" = Just 12
codeToIndex "TT" = Just 13
codeToIndex "FT" = Just 14
codeToIndex "FF" = Just 15
codeToIndex _ = Nothing

indexToLetter i = toEnum (fromEnum 'A' + i)
indexToDigit i = toEnum (fromEnum '1' + i)
indexToChar i
  | i < 9 = indexToDigit i
  | otherwise = indexToLetter (i - 9)


readExact expected actual
  | expected == actual = \rd -> return rd
  | otherwise = const $ Err $ "expected exactly " ++ show expected ++ ", but got " ++ show actual


initCommon name aliases init_detail = HlRecord
    { _hlr_NAME = name
    , _hlr_FLNK = NoRLink
    , _hlr_PINI = False
    , _hlr_SCAN = Scan_Passive
    , _hlr_ASG = "DEFAULT"
    , _hlr_SDIS = NoLink
    , _hlr_DISA = 0
    , _hlr_DISV = 1
    , _hlr_aliases = aliases
    , _detail = init_detail
    }

updateCommon :: String -> String -> HlRecord -> M HlRecord
updateCommon fn fv rc = case fn of
    "FLNK" -> readRecordLink fv >>= \lnk -> return $ set hlr_FLNK lnk rc
    "PINI" -> case fv of
        "0" -> return $ set hlr_PINI False rc
        "1" -> return $ set hlr_PINI True rc
        _ -> Err $ "unexpected PINI value: " ++ show fv
    "SCAN" -> case fv of
        "Passive" -> return $ set hlr_SCAN Scan_Passive rc
        ".1 second" -> return $ set hlr_SCAN (Scan_Periodic 0.1) rc
        ".2 second" -> return $ set hlr_SCAN (Scan_Periodic 0.2) rc
        ".5 second" -> return $ set hlr_SCAN (Scan_Periodic 0.5) rc
        "1 second" -> return $ set hlr_SCAN (Scan_Periodic 1.0) rc
        "2 second" -> return $ set hlr_SCAN (Scan_Periodic 2.0) rc
        "5 second" -> return $ set hlr_SCAN (Scan_Periodic 5.0) rc
        "10 second" -> return $ set hlr_SCAN (Scan_Periodic 10.0) rc
        "I/O Intr" -> return $ set hlr_SCAN Scan_IOIntr rc
        _ -> Err $ "unexpected SCAN value: " ++ show fv
    "ASG" -> setM hlr_ASG (return fv) rc
    "SDIS" -> setM hlr_SDIS (readFieldLink fv) rc
    -- TODO: what is the purpose of setting DISA? won't it be overwritten
    -- immediately?
    "DISA" -> setM hlr_DISV (readInt fv) rc
    "DISV" -> setM hlr_DISV (readInt fv) rc

    _ -> TryNext


updateDisplayParams fn fv rc = case fn of
    -- EGU (Engineering Units), HOPR (High Operating Range), LOPR (Low
    -- Operating Range), PREC (Display Precision), DESC (Description): all of
    -- thees are used for display only.
    "EGU" -> return rc
    "HOPR" -> return rc
    "LOPR" -> return rc
    "PREC" -> return rc
    "DESC" -> return rc
    _ -> TryNext

initAlarm = HlAlarm
    { _alarm_HIHI = 0
    , _alarm_HIGH = 0
    , _alarm_LOW = 0
    , _alarm_LOLO = 0
    , _alarm_HHSV = AlarmSevr_NO_ALARM
    , _alarm_HSV = AlarmSevr_NO_ALARM
    , _alarm_LSV = AlarmSevr_NO_ALARM
    , _alarm_LLSV = AlarmSevr_NO_ALARM
    }

readAlarmSevr s = case s of
    "NO_ALARM" -> return AlarmSevr_NO_ALARM
    "MINOR" -> return AlarmSevr_MINOR
    "MAJOR" -> return AlarmSevr_MAJOR
    "INVALID" -> return AlarmSevr_INVALID
    _ -> Err $ "invalid alarm severity: "++ show s

updateAlarm fn fv = case fn of
    "HIHI" -> setM alarm_HIHI (readDouble fv)
    "HIGH" -> setM alarm_HIGH (readDouble fv)
    "LOW" -> setM alarm_LOW (readDouble fv)
    "LOLO" -> setM alarm_LOLO (readDouble fv)
    "HHSV" -> setM alarm_HHSV (readAlarmSevr fv)
    "HSV" -> setM alarm_HSV (readAlarmSevr fv)
    "LSV" -> setM alarm_LSV (readAlarmSevr fv)
    "LLSV" -> setM alarm_LLSV (readAlarmSevr fv)
    _ -> const TryNext


init_calc = Calc
    { _calc_CALC = CE.dummyExp
    , _calc_INPA_to_INPL = multiRep NoLink
    , _calc_A_to_L = multiRep (0 :: Double)
    , _calc_VAL = 0
    }

update_calc fn fv = case fn of
    "CALC" -> setM calc_CALC (readExp fv >>= readCalcExp)
    ['I','N','P',x] | Just i <- letterToIndex 12 x ->
        setM (calc_INPA_to_INPL . idx i) (readFieldLink fv)
    [x] | Just i <- letterToIndex 12 x ->
        setM (calc_A_to_L . idx i) (readDouble fv)
    "VAL" -> setM calc_VAL (readDouble fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        ]


readConv s = case s of
    "NO CONVERSION" -> return Conv_None
    "SLOPE" -> return Conv_Slope
    "LINEAR" -> return Conv_Linear
    _ -> Err $ "unexpected LINR value: " ++ show s

init_ao = AnalogOut
    { _ao_DOL = NoLink
    , _ao_VAL = 0
    , _ao_OMSL = Omsl_Supervisory
    , _ao_DTYP = defaultDtyp
    , _ao_LINR = Conv_None
    , _ao_ESLO = 1.0
    , _ao_EOFF = 0.0
    , _ao_alarm = initAlarm
    }

update_ao fn fv = case fn of
    "DOL" -> setM ao_DOL (readFieldLink fv)
    "VAL" -> setM ao_VAL (readDouble fv)
    "OMSL" -> setM ao_OMSL (readOmsl fv)
    "DTYP" -> setM ao_DTYP (readDeviceType fv)
    "LINR" -> setM ao_LINR (readConv fv)
    "ESLO" -> setM ao_ESLO (readDouble fv)
    "EOFF" -> setM ao_EOFF (readDouble fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        , overM ao_alarm (updateAlarm fn fv) rd
        ]


init_ai = AnalogIn
    { _ai_INP = NoLink
    , _ai_VAL = 0
    , _ai_DTYP = defaultDtyp
    , _ai_LINR = Conv_None
    , _ai_ESLO = 1.0
    , _ai_EOFF = 1.0
    , _ai_alarm = initAlarm
    }

ensureHwLink x@(NoLink) = return x
ensureHwLink x@(HardwareAddr _) = return x
ensureHwLink x = Err $ "expected device link, not " ++ show x

update_ai fn fv = case fn of
    "INP" -> setM ai_INP (readFieldLink fv >>= ensureHwLink)
    "VAL" -> setM ai_VAL (readDouble fv)
    "DTYP" -> setM ai_DTYP (readDeviceType fv)
    "LINR" -> setM ai_LINR (readConv fv)
    "ESLO" -> setM ai_ESLO (readDouble fv)
    "EOFF" -> setM ai_EOFF (readDouble fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        , overM ai_alarm (updateAlarm fn fv) rd
        ]


readOOpt s = case s of
    "Every Time" -> return OOpt_EveryTime
    "On Change" -> return OOpt_OnChange
    "When Zero" -> return OOpt_WhenZero
    "When Non-zero" -> return OOpt_WhenNonzero
    "Transition To Zero" -> return OOpt_TransitionToZero
    "Transition To Non-zero" -> return OOpt_TransitionToNonzero
    _ -> Err $ "unexpected OOPT value: " ++ show s

readDOpt s = case s of
    "Use CALC" -> return DOpt_UseCALC
    "Use OCAL" -> return DOpt_UseOCAL
    _ -> Err $ "unexpected DOPT value: " ++ show s


init_calcout = CalcOut
    { _calcout_CALC = CE.dummyExp
    , _calcout_OCAL = CE.dummyExp
    , _calcout_INPA_to_INPL = multiRep NoLink
    , _calcout_OUT = NoLink
    , _calcout_OOPT = OOpt_EveryTime
    , _calcout_DOPT = DOpt_UseCALC
    , _calcout_A_to_L = multiRep (0 :: Double)
    , _calcout_VAL = 0
    , _calcout_OVAL = 0
    }

update_calcout fn fv = case fn of
    "CALC" -> setM calcout_CALC (readExp fv >>= readCalcExp)
    "OCAL" -> setM calcout_OCAL (readExp fv >>= readCalcExp)
    ['I','N','P',x] | Just i <- letterToIndex 12 x ->
        setM (calcout_INPA_to_INPL . idx i) (readFieldLink fv)
    "OUT" -> setM calcout_OUT (readFieldLink fv)
    "OOPT" -> setM calcout_OOPT (readOOpt fv)
    "DOPT" -> setM calcout_DOPT (readDOpt fv)
    [x] | Just i <- letterToIndex 12 x ->
        setM (calcout_A_to_L . idx i) (readDouble fv)
    "VAL" -> setM calcout_VAL (readDouble fv)
    "OVAL" -> setM calcout_OVAL (readDouble fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        ]


init_scalcout = StrCalcOut
    { _scalcout_CALC = CE.dummyExp
    , _scalcout_OCAL = CE.dummyExp
    , _scalcout_INPA_to_INPL = multiRep NoLink
    , _scalcout_INAA_to_INLL = multiRep NoLink
    , _scalcout_OUT = NoLink
    , _scalcout_OOPT = OOpt_EveryTime
    , _scalcout_DOPT = DOpt_UseCALC
    , _scalcout_A_to_L = multiRep (0 :: Double)
    , _scalcout_AA_to_LL = multiRep ""
    , _scalcout_VAL = 0
    , _scalcout_SVAL = ""
    , _scalcout_OVAL = 0
    , _scalcout_OSV = ""
    }

update_scalcout fn fv = case fn of
    "CALC" -> setM scalcout_CALC (readExp fv >>= readCalcExp)
    "OCAL" -> setM scalcout_OCAL (readExp fv >>= readCalcExp)
    ['I','N','P',x] | Just i <- letterToIndex 12 x ->
        setM (scalcout_INPA_to_INPL . idx i) (readFieldLink fv)
    ['I','N',x,x'] | x == x', Just i <- letterToIndex 12 x ->
        setM (scalcout_INAA_to_INLL . idx i) (readFieldLink fv)
    "OUT" -> setM scalcout_OUT (readFieldLink fv)
    "OOPT" -> setM scalcout_OOPT (readOOpt fv)
    "DOPT" -> setM scalcout_DOPT (readDOpt fv)
    [x] | Just i <- letterToIndex 12 x ->
        setM (scalcout_A_to_L . idx i) (readDouble fv)
    [x,x'] | x == x', Just i <- letterToIndex 12 x ->
        setM (scalcout_AA_to_LL . idx i) (return fv)
    "VAL" -> setM scalcout_VAL (readDouble fv)
    "SVAL" -> setM scalcout_SVAL (return fv)
    "OVAL" -> setM scalcout_OVAL (readDouble fv)
    "OSV" -> setM scalcout_OSV (return fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        ]


init_acalcout = ArrayCalcOut
    { _acalcout_NELM = 1
    , _acalcout_CALC = CE.dummyExp
    , _acalcout_OCAL = CE.dummyExp
    , _acalcout_INPA_to_INPL = multiRep NoLink
    , _acalcout_INAA_to_INLL = multiRep NoLink
    , _acalcout_OUT = NoLink
    , _acalcout_OOPT = OOpt_EveryTime
    , _acalcout_DOPT = DOpt_UseCALC
    , _acalcout_A_to_L = multiRep (0 :: Double)
    , _acalcout_AA_to_LL = multiRep ([] :: [Double])
    , _acalcout_VAL = 0
    , _acalcout_AVAL = []
    , _acalcout_OVAL = 0
    , _acalcout_OAV = []
    }

update_acalcout fn fv = case fn of
    "NELM" -> setM acalcout_NELM (readInt fv)
    "CALC" -> setM acalcout_CALC (readExp fv >>= readCalcExp)
    "OCAL" -> setM acalcout_OCAL (readExp fv >>= readCalcExp)
    ['I','N','P',x] | Just i <- letterToIndex 12 x ->
        setM (acalcout_INPA_to_INPL . idx i) (readFieldLink fv)
    ['I','N',x,x'] | x == x', Just i <- letterToIndex 12 x ->
        setM (acalcout_INAA_to_INLL . idx i) (readFieldLink fv)
    "OUT" -> setM acalcout_OUT (readFieldLink fv)
    "OOPT" -> setM acalcout_OOPT (readOOpt fv)
    "DOPT" -> setM acalcout_DOPT (readDOpt fv)
    [x] | Just i <- letterToIndex 12 x ->
        setM (acalcout_A_to_L . idx i) (readDouble fv)
    "VAL" -> setM acalcout_VAL (readDouble fv)
    "OVAL" -> setM acalcout_OVAL (readDouble fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        ]


init_bi = BinaryIn
    { _bi_INP = NoLink
    , _bi_VAL = 0
    , _bi_ZNAM_ONAM = multiRep ""
    , _bi_DTYP = defaultDtyp
    , _bi_ZSV_OSV = multiRep AlarmSevr_NO_ALARM
    }

update_bi fn fv = case fn of
    "INP" -> setM bi_INP (readFieldLink fv >>= ensureHwLink)
    "VAL" -> setM bi_VAL (readInt fv)
    "ZNAM" -> setM (bi_ZNAM_ONAM . idx 0) (return fv)
    "ONAM" -> setM (bi_ZNAM_ONAM . idx 1) (return fv)
    "DTYP" -> setM bi_DTYP (readDeviceType fv)
    "ZSV" -> setM (bi_ZSV_OSV . idx 0) (readAlarmSevr fv)
    "OSV" -> setM (bi_ZSV_OSV . idx 1) (readAlarmSevr fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        ]


init_bo = BinaryOut
    { _bo_DOL = NoLink
    , _bo_VAL = 0
    , _bo_ZNAM_ONAM = multiRep ""
    , _bo_OMSL = Omsl_Supervisory
    , _bo_DTYP = defaultDtyp
    , _bo_ZSV_OSV = multiRep AlarmSevr_NO_ALARM
    }

readOmsl s = case s of
    "supervisory" -> return Omsl_Supervisory
    "closed_loop" -> return Omsl_ClosedLoop
    _ -> Err $ "unexpected OMSL value: " ++ show s

update_bo fn fv = case fn of
    "DOL" -> setM bo_DOL (readFieldLink fv)
    "VAL" -> setM bo_VAL (readInt fv)
    "ZNAM" -> setM (bo_ZNAM_ONAM . idx 0) (return fv)
    "ONAM" -> setM (bo_ZNAM_ONAM . idx 1) (return fv)
    "OMSL" -> setM bo_OMSL (readOmsl fv)
    "DTYP" -> setM bo_DTYP (readDeviceType fv)
    "ZSV" -> setM (bi_ZSV_OSV . idx 0) (readAlarmSevr fv)
    "OSV" -> setM (bi_ZSV_OSV . idx 1) (readAlarmSevr fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        ]


init_mbbo = MBBO
    { _mbbo_DOL = NoLink
    , _mbbo_VAL = 0
    , _mbbo_ZRST_to_FFST = multiRep ""
    , _mbbo_OMSL = Omsl_Supervisory
    , _mbbo_ZRSV_to_FFSV = multiRep AlarmSevr_NO_ALARM
    , _mbbo_DTYP = defaultDtyp
    }

update_mbbo fn fv = case fn of
    "DOL" -> setM mbbo_DOL (readFieldLink fv)
    "VAL" -> setM mbbo_VAL (readInt fv)
    [x,y,'S','T'] | Just i <- codeToIndex [x,y] ->
        setM (mbbo_ZRST_to_FFST . idx i) (return fv)
    "OMSL" -> setM mbbo_OMSL (readOmsl fv)
    [x,y,'S','V'] | Just i <- codeToIndex [x,y] ->
        setM (mbbo_ZRSV_to_FFSV . idx i) (readAlarmSevr fv)
    "DTYP" -> setM mbbo_DTYP (readDeviceType fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        ]


init_stringin = StringIn
    { _stringin_INP = NoLink
    , _stringin_VAL = ""
    , _stringin_alarm = initAlarm
    , _stringin_DTYP = defaultDtyp
    }

update_stringin fn fv = case fn of
    "INP" -> setM stringin_INP (readFieldLink fv >>= ensureHwLink)
    "VAL" -> setM stringin_VAL (return fv)
    "DTYP" -> setM stringin_DTYP (readDeviceType fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        , overM stringin_alarm (updateAlarm fn fv) rd
        ]


init_stringout = StringOut
    { _stringout_DOL = NoLink
    , _stringout_VAL = ""
    , _stringout_OMSL = Omsl_Supervisory
    , _stringout_DTYP = defaultDtyp
    }

update_stringout fn fv = case fn of
    "DOL" -> setM stringout_DOL (readFieldLink fv)
    "VAL" -> setM stringout_VAL (return fv)
    "OMSL" -> setM stringout_OMSL (readOmsl fv)
    "DTYP" -> setM stringout_DTYP (readDeviceType fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        ]


init_longin = LongIn
    { _longin_INP = NoLink
    , _longin_VAL = 0
    , _longin_alarm = initAlarm
    , _longin_DTYP = defaultDtyp
    }

update_longin fn fv = case fn of
    "INP" -> setM longin_INP (readFieldLink fv >>= ensureHwLink)
    "VAL" -> setM longin_VAL (readInt32 fv)
    "DTYP" -> setM longin_DTYP (readDeviceType fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        , overM longin_alarm (updateAlarm fn fv) rd
        ]


init_longout = LongOut
    { _longout_DOL = NoLink
    , _longout_VAL = 0
    , _longout_OMSL = Omsl_Supervisory
    , _longout_DTYP = defaultDtyp
    }

update_longout fn fv = case fn of
    "DOL" -> setM longout_DOL (readFieldLink fv)
    "VAL" -> setM longout_VAL (readInt32 fv)
    "OMSL" -> setM longout_OMSL (readOmsl fv)
    "DTYP" -> setM longout_DTYP (readDeviceType fv)
    _ -> \rd -> msum
        [ updateDisplayParams fn fv rd
        ]


readFtype s = case s of
    "LONG" -> return Ftype_Long
    "SHORT" -> return Ftype_Short
    _ -> Err $ "unexpected Ftype value: " ++ show s

init_waveform = Waveform
    { _waveform_DTYP = defaultDtyp
    , _waveform_INP = NoLink
    -- TODO make sure this is the right default
    , _waveform_FTVL = Ftype_String
    , _waveform_NELM = 1
    }

update_waveform fn fv = case fn of
    "DTYP" -> setM waveform_DTYP (readDeviceType fv)
    "INP" -> setM waveform_INP (readFieldLink fv >>= ensureHwLink)
    "FTVL" -> setM waveform_FTVL (readFtype fv)
    "NELM" -> setM waveform_NELM (readInt fv)
    _ -> \rd -> msum
        [
        ]


init_subarray = SubArray
    { _subarray_INP = NoLink
    , _subarray_MALM = 1
    , _subarray_NELM = 1
    , _subarray_INDX = 0
    -- TODO make sure this is the right default
    , _subarray_FTVL = Ftype_String
    }

update_subarray fn fv = case fn of
    "INP" -> setM subarray_INP (readFieldLink fv)
    "MALM" -> setM subarray_MALM (readInt fv)
    "NELM" -> setM subarray_NELM (readInt fv)
    "INDX" -> setM subarray_INDX (readInt fv)
    "FTVL" -> setM subarray_FTVL (readFtype fv)
    _ -> \rd -> msum
        [
        ]


init_fanout = Fanout
    { _fanout_LNK1_to_LNK6 = multiRep NoRLink
    }

update_fanout fn fv = case fn of
    ['L','N','K',x] | Just i <- digitToIndex 6 x ->
        setM (fanout_LNK1_to_LNK6 . idx i) (readRecordLink fv)
    "SELM" -> case fv of
        "All" -> return
        _ -> const $ Err $ "unexpected SELM value: " ++ show fv
    _ -> \rd -> msum
        [
        ]


init_dfanout = DFanout
    { _dfanout_DOL = NoLink
    , _dfanout_OUTA_to_OUTH = multiRep NoLink
    , _dfanout_OMSL = Omsl_Supervisory
    , _dfanout_VAL = 0
    }

update_dfanout fn fv = case fn of
    "DOL" -> setM dfanout_DOL (readFieldLink fv)
    ['O','U','T',x] | Just i <- letterToIndex 8 x ->
        setM (dfanout_OUTA_to_OUTH . idx i) (readFieldLink fv)
    "OMSL" -> setM dfanout_OMSL (readOmsl fv)
    "VAL" -> setM dfanout_VAL (readDouble fv)
    "SELM" -> case fv of
        "All" -> return
        _ -> const $ Err $ "unexpected SELM value: " ++ show fv
    -- SELL + SELN can be ignored because we require SELM = All
    "SELL" -> return
    "SELN" -> return
    _ -> \rd -> msum
        [
        ]


init_seq = Seq
    { _seq_SELM = SelM_All
    , _seq_SELL = NoLink
    , _seq_DOL1_to_DOLA = multiRep NoLink
    , _seq_DO1_to_DOA = multiRep (0 :: Double)
    , _seq_LNK1_to_LNKA = multiRep NoLink
    , _seq_DLY1_to_DLYA = multiRep (0 :: Double)
    }

update_seq :: String -> String -> HlDetail -> M HlDetail
update_seq fn fv = case fn of
    "SELM" -> case fv of
        "All" -> setM seq_SELM (return SelM_All)
        "Specified" -> setM seq_SELM (return SelM_Specified)
        _ -> const $ Err $ "unexpected SELM value: " ++ show fv
    "SELL" -> setM seq_SELL (readFieldLink fv)
    ['D','O','L',x] | Just i <- charToIndex 10 x ->
        setM (seq_DOL1_to_DOLA . idx i) (readFieldLink fv)
    ['D','O',x] | Just i <- charToIndex 10 x ->
        setM (seq_DO1_to_DOA . idx i) (readDouble fv)
    ['L','N','K',x] | Just i <- charToIndex 10 x ->
        setM (seq_LNK1_to_LNKA . idx i) (readFieldLink fv)
    ['D','L','Y',x] | Just i <- charToIndex 10 x ->
        setM (seq_DLY1_to_DLYA . idx i) (readDouble fv)
    _ -> \rd -> msum
        [
        ]


init_asyn = Asyn {}

update_asyn fn fv = case fn of
    -- All of these are known not to lead toinput/output events or interactions
    -- with the rest of the database.
    "DTYP" -> return
    "PORT" -> return
    "ADDR" -> return
    "OMAX" -> return
    "IMAX" -> return
    _ -> \rd -> msum
        [
        ]


init_havoc = Havoc
    { _havoc_in_links = []
    , _havoc_out_links = []
    , _havoc_fwd_links = []
    }

update_havoc rty fn fv r = do
    ftys <- case M.lookup rty fieldtypes of
        Just x -> return x
        Nothing -> Err $ "no field info for record type: " ++ show rty
    fty <- case M.lookup fn ftys of
        Just x -> return x
        Nothing -> Err $ "no field info for field: " ++ rty ++ "." ++ fn
    case fty of
        INLINK -> overM havoc_in_links (\ls -> do l <- readFieldLink fv; return (l:ls)) r
        OUTLINK -> overM havoc_out_links (\ls -> do l <- readFieldLink fv; return (l:ls)) r
        FWDLINK -> overM havoc_fwd_links (\ls -> do l <- readRecordLink fv; return (l:ls)) r
        _ -> return r


data EpicsType = TDouble | TString | TInt | TBool
  deriving (Eq, Ord, Show)

data InLinkDef = InLinkDef
    { _ild_name :: String
    , _ild_lens :: forall f. Applicative f =>
        ((FieldLink -> f FieldLink) -> (HlDetail -> f HlDetail))
    , _ild_type :: EpicsType
    }
makeLenses ''InLinkDef

inLinkList :: [(HlRecordType, [InLinkDef])]
inLinkList =
    [ (RtCalc,
        [ InLinkDef ("INP" ++ [indexToLetter i]) (calc_INPA_to_INPL . idx i) TDouble
            | i <- [0 .. 11]])
    , (RtCalcOut,
        [ InLinkDef ("INP" ++ [indexToLetter i]) (calcout_INPA_to_INPL . idx i) TDouble
            | i <- [0 .. 11]])
    , (RtStrCalcOut,
        [ InLinkDef ("INP" ++ [indexToLetter i]) (scalcout_INPA_to_INPL . idx i) TDouble
            | i <- [0 .. 11]] ++
        [ InLinkDef ("IN" ++ let c = indexToLetter i in [c,c]) (scalcout_INAA_to_INLL . idx i) TString
            | i <- [0 .. 11]])
    , (RtArrayCalcOut,
        [ InLinkDef ("INP" ++ [indexToLetter i]) (acalcout_INPA_to_INPL . idx i) TDouble
            | i <- [0 .. 11]] ++
        [ InLinkDef ("IN" ++ let c = indexToLetter i in [c,c]) (acalcout_INAA_to_INLL . idx i) TString
            | i <- [0 .. 11]])
    , (RtAnalogOut, [InLinkDef "DOL" ao_DOL TDouble])
    , (RtAnalogIn, [InLinkDef "INP" ai_INP TDouble])
    , (RtBinaryIn, [InLinkDef "INP" bi_INP TBool])
    , (RtBinaryOut, [InLinkDef "DOL" bo_DOL TBool])
    , (RtMBBO, [InLinkDef "DOL" mbbo_DOL TInt])
    , (RtStringIn, [InLinkDef "INP" stringin_INP TString])
    , (RtStringOut, [InLinkDef "DOL" stringout_DOL TString])
    , (RtLongIn, [InLinkDef "INP" longin_INP TInt])
    , (RtLongOut, [InLinkDef "DOL" longout_DOL TInt])
    , (RtWaveform, [])
    -- TODO: add subarray.INP (type = array)
    , (RtSubArray, [])
    , (RtFanout, [])
    , (RtDFanout, [InLinkDef "DOL" dfanout_DOL TDouble])
    , (RtSeq,
        [ InLinkDef ("DOL" ++ [indexToChar i]) (seq_DOL1_to_DOLA . idx i) TDouble
            | i <- [0 .. 9]])
    , (RtAsyn, [])
    , (RtHavoc, [])
    ]

inLinkMap :: M.Map HlRecordType (M.Map String InLinkDef)
inLinkMap =
    M.fromList [(rn, ildMap) | (rn, ilds) <- inLinkList,
        ildMap <- [M.fromList [(ild ^. ild_name, ild) | ild <- ilds]]
    ]

hlRecordType :: HlDetail -> HlRecordType
hlRecordType Calc{} = RtCalc
hlRecordType CalcOut{} = RtCalcOut
hlRecordType StrCalcOut{} = RtStrCalcOut
hlRecordType ArrayCalcOut{} = RtArrayCalcOut
hlRecordType AnalogOut{} = RtAnalogOut
hlRecordType AnalogIn{} = RtAnalogIn
hlRecordType BinaryIn{} = RtBinaryIn
hlRecordType BinaryOut{} = RtBinaryOut
hlRecordType MBBO{} = RtMBBO
hlRecordType StringIn{} = RtStringIn
hlRecordType StringOut{} = RtStringOut
hlRecordType LongIn{} = RtLongIn
hlRecordType LongOut{} = RtLongOut
hlRecordType Waveform{} = RtWaveform
hlRecordType SubArray{} = RtSubArray
hlRecordType Fanout{} = RtFanout
hlRecordType DFanout{} = RtDFanout
hlRecordType Seq{} = RtSeq
hlRecordType Asyn{} = RtAsyn
hlRecordType Havoc{} = RtHavoc



-- For each input links that refers to a missing record, generate a new havoc
-- record with that name.
fixMissingRecords db = db `M.union` dbExt
  where
    knownRecords = S.fromList $ M.keys db
    usedRecords = S.fromList $ do
        (rn, r) <- M.toList db
        FieldLink { fl_record = target } <- 
            case r ^. detail of
                d@Havoc{} -> d ^. havoc_in_links
                d -> do
                    ild <- map snd $ M.toList $ inLinkMap M.! hlRecordType d
                    [d ^. _ild_lens ild]
        return target
    needRecords = usedRecords `S.difference` knownRecords
    dbExt = M.fromList [(rn, initCommon rn [] init_havoc) | rn <- S.toList needRecords]



data FixHavocState = FixHavocState
    { _fhs_rtys :: M.Map String HlRecordType
    , _fhs_used :: M.Map (String, EpicsType) Int
    , _fhs_map :: M.Map (String, String) String
    }
makeLenses ''FixHavocState

type FixHavocM a = State FixHavocState a

havocTypedField = M.fromList
    [ (TDouble, "H_DBL")
    , (TString, "H_STR")
    , (TBool, "H_BOOL")
    , (TInt, "H_INT")
    ]

-- Generate a new field name to replace field `fn` of (havoc) record `rn`.  The
-- field will contain a value of type `fty`.
assignNewField rn fn fty = do
    let base = havocTypedField M.! fty
    nextIdx <- use $ fhs_used . at (rn, fty) . _Just
    fhs_used . at (rn, fty) . _Just %= (+1)
    let newFn = base ++ "_" ++ show nextIdx

    fhs_map . at (rn, fn) .= Just newFn
    return newFn

-- Rewrite an INLINK expecting a value of the indicated type.
fixHavocInLink :: EpicsType -> FieldLink -> FixHavocM FieldLink
fixHavocInLink fty fl@FieldLink{} = do
    let rn = fl_record fl
        fn = fl_field fl
    rty <- use $ fhs_rtys . at rn . _Just
    if rty /= RtHavoc then return fl else do
      mNewFn <- use $ fhs_map . at (rn, fn)
      newFn <- case mNewFn of
          Just newFn -> return newFn
          Nothing -> assignNewField rn fn fty
      return $ fl { fl_field = newFn }
fixHavocInLink _ fl = return fl

fixHavocRecordLinks :: HlDetail -> FixHavocM HlDetail
fixHavocRecordLinks r@Havoc{} = do
    let ils = r ^. havoc_in_links
    let fls = [RecordLink target | FieldLink { fl_record = target } <- ils]
    let r' = r & havoc_in_links .~ []
        r'' = r' & havoc_fwd_links %~ (++ fls)
    return r''
fixHavocRecordLinks r = do
    let rty = hlRecordType r
    let ilds = [v | (k,v) <- M.toList $ inLinkMap M.! rty]
    foldM (\r ild -> overM (_ild_lens ild) (fixHavocInLink (ild ^. ild_type)) r) r ilds

initFixHavocState db = FixHavocState
    { _fhs_rtys = rtys
    , _fhs_used = used
    , _fhs_map = M.empty
    }
  where
    rtys = M.map (\r -> hlRecordType $ r ^. detail) db
    used = M.fromList [((rn, fty), 0) |
        (rn, rty) <- M.toList rtys,
        rty == RtHavoc,
        (fty, _) <- M.toList havocTypedField]

-- Find input links that read from a havoc record, and change the target field
-- to one of the H_* fields.
fixHavoc :: HlDatabase -> HlDatabase
fixHavoc db = flip evalState (initFixHavocState db) $ do
    forM db $ \r -> do
        d' <- fixHavocRecordLinks $ r ^. detail
        return $ r & detail .~ d'
