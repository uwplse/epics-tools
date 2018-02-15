module Fields where

{-

This module has a bunch of stuff related to getting field types and values of
records and parsing said values.

-}

import qualified Data.Map as M
import Data.Maybe(isJust)
import Data.Char(isSpace)
import Data.List(isSuffixOf)

import FieldInfo
import Parser hiding (parens)
import Record

allFields :: Record -> M.Map FieldName FieldType
allFields r =
    case M.lookup (substStringToStr $ r_type r) fieldtypes of
        Just fields -> fields
        _ -> error $ "unknown record type " ++ substStringToStr (r_type r)

hasField :: Record -> FieldName -> Bool
hasField r f = isJust (M.lookup f (allFields r))

-- Does this record define a value for the field? If this returns false, then
-- the default value for the field will be used by readField.
definesValueForField :: Record -> FieldName -> Bool
definesValueForField r fieldName = isJust (lookup fieldName $ r_fields r)

typeOfField :: Record -> FieldName -> FieldType
typeOfField r f
    | (Just ty) <- M.lookup f (allFields r) = ty
    | otherwise = error $ "unknown field " ++ f ++ " on record of type " ++ (substStringToStr $ r_type r)

readField :: Record -> FieldName -> String
readField r fieldName = readFieldWithDefault r fieldName (defaultValue r fieldName)

readFieldWithDefault :: Record -> FieldName -> String -> String
readFieldWithDefault r fieldName defaultVal =
    case (lookup fieldName $ r_fields r) of
        Just [Literal val] -> val
        Nothing -> defaultVal

defaultValue :: Record -> FieldName -> String
defaultValue r f
    -- TODO: is this totally right?
    | typeOfField r f `elem` [LONG, ULONG, CHAR, UCHAR, SHORT, USHORT, DOUBLE] = "0"
    | MENU options <- typeOfField r f = head options
    | otherwise = ""

-------------------------------------------------------------------------------

type EventID = Integer
data Priority = Low | Medium | High
  deriving (Eq, Ord, Show)

data ScanMode =
  Periodic Rational |
  Event EventID Priority |
  Interrupt |
  Passive
  deriving (Eq, Ord, Show)

phase r =
  case maybeRead (readField r "PHAS") :: Maybe Integer of
    Just i -> i
    Nothing -> error $ "failed to parse PHAS field of record " ++ show r

scanMode r =
  case readField r "SCAN" of
    "Passive" -> Passive
    "I/O Intr" -> Interrupt
    "Event" -> case maybeRead (readField r "EVNT") :: Maybe Integer of
      Just i ->
        case readField r "PRIO" of
          "LOW" -> Event i Low
          "MEDIUM" -> Event i Medium
          "HIGH" -> Event i High
          _ -> error $ "failed to parse PRIO field of record " ++ show r
      _ -> error $ "failed to parse EVNT field of record " ++ show r
    periodic | "second" `isSuffixOf` periodic, Just rate <- maybeReadRational ('0' : takeWhile (not.isSpace) periodic) -> Periodic rate
    _ -> error $ "failed to parse SCAN field of record " ++ show r

isPassive = (== Passive) . scanMode
