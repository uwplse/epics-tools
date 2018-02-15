module Lint(LintContext(..), LintWarning(..), lint) where

import qualified Data.Map as M

import Record
import FieldInfo
import Fields
import Parser(StringPart(..))

data LintContext =
  LintContext {
    ca_records :: [RecordName] }

data LintWarning =
  MissingCAAnnotation RecordName FieldName String |
  BrokenFieldLink RecordName FieldName String |
  BrokenRecordLink RecordName FieldName String

instance Show LintWarning where
  show (MissingCAAnnotation src srcField val) =
    "Missing channel access annotation: " ++ recordNameToStr src ++ "." ++ srcField ++ " links to missing field " ++ val
  show (BrokenFieldLink src srcField val) =
    "Broken link: " ++ recordNameToStr src ++ "." ++ srcField ++ " links to missing field " ++ val
  show (BrokenRecordLink src srcField val) =
    "Broken link: " ++ recordNameToStr src ++ "." ++ srcField ++ " links to missing record " ++ val

ok = [] -- no warnings

lint :: LintContext -> Database -> [LintWarning]
lint ctx db =
  concatMap (brokenLinks (ca_records ctx) db) db

brokenLinks :: [RecordName] -> Database -> Record -> [LintWarning]
brokenLinks ca_records db r =
  concatMap checkLink (M.toList $ allFields r)
  where
    checkLink (f, ty) | ty `elem` [INLINK, OUTLINK] =
      let val = readField r f in
      case parseFieldLink (readField r f) of
        NoLink -> ok
        HardwareAddr _ -> ok
        FieldLink { fl_record = rn, fl_field = ff } ->
          let rname = [Literal rn] in
          case findRecord rname db of
            Nothing
              | rname `elem` ca_records -> [MissingCAAnnotation (r_name r) f val]
              | otherwise -> [BrokenRecordLink (r_name r) f val]
            Just rr | not (hasField rr ff) -> [BrokenFieldLink (r_name r) f val]
            _ -> ok
        Constant _ ->
          if ty == INLINK
            then ok
            else [BrokenFieldLink (r_name r) f (readField r f)]
    checkLink (f, FWDLINK) =
      let val = readField r f in
      case parseRecordLink val of
        Just rname
          | rname `elem` ca_records ->
            [MissingCAAnnotation (r_name r) f val]
          | otherwise ->
            case findRecord rname db of
              Nothing -> [BrokenRecordLink (r_name r) f val]
              _ -> ok
        _ -> ok
    checkLink _ = ok
