{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, FlexibleContexts #-}
module Record
where

import Control.Applicative ((<$>))
import Control.Monad (msum)
import Data.Data
import Data.Either (partitionEithers)
import Data.List (find, isPrefixOf)
import Text.Parsec hiding (label, State, string, space)

import Parser
import Util

-- convenience types
type Database = [Record]
type RecordName = SubstString
type FieldName = Ident

data Record = Record
    { r_name :: RecordName
    , r_type :: SubstString
    , r_fields :: [(FieldName, SubstString)]
    , r_aliases :: [RecordName]
    }
  deriving (Show, Data, Typeable)

record = do
    exactWord "record"
    (ty, name) <- parens $ do
        ty <- dbValue
        comma
        name <- dbValue
        return (ty, name)
    (fields, aliases) <- braces $ partitionEithers <$> many
            (Left <$> field <|> Right <$> aliasInternal)
    return $ Record { r_name = name, r_type = ty, r_fields = fields, r_aliases = aliases }

field = do
    exactWord "field"
    parens $ do
        name <- word
        comma
        arg <- dbValue
        optionalSpaces
        return (name, arg)

aliasInternal = do
    exactWord "alias"
    name <- parens $ dbValue
    return name

printRecord r = header ++ fields ++ aliases
  where
    header = showSubst (r_type r) ++ " " ++ showSubst (r_name r) ++ "\n"

    fields = concatMap go (r_fields r)
    go (k,v) = "  " ++ showSubst (r_type r) ++ "." ++ k ++ " = " ++ showSubst v ++ "\n"

    aliases = concatMap (\a -> "  alias " ++ showSubst a ++ "\n") (r_aliases r)

    showSubst [Literal s] = s
    showSubst ss = show ss


data PpFlag =
      NoProcessPassive
    | ProcessPassive
  deriving (Eq, Show, Data, Typeable)

data MsFlag =
      NonMaximizeSeverity
    | MaximizeSeverity
    | MaximizeStatusAndSeverity
    | MaximizeSeverityIfInvalid
  deriving (Eq, Show, Data, Typeable)

data FieldLink =
    NoLink |
    HardwareAddr Ident |
    Constant Double |
    FieldLink
    { fl_record :: String
    , fl_field :: String
    , fl_pp :: PpFlag
    , fl_ms :: MsFlag
    }
  deriving (Show, Data, Typeable)

data RecordLink =
    NoRLink |
    RecordLink
    { rl_record :: String
    }
  deriving (Show, Data, Typeable)

parsePpFlag "NPP" = Just NoProcessPassive
parsePpFlag "PP" = Just ProcessPassive
parsePpFlag _ = Nothing

parseMsFlag "NMS" = Just NonMaximizeSeverity
parseMsFlag "MS" = Just MaximizeSeverity
parseMsFlag "MSS" = Just MaximizeStatusAndSeverity
parseMsFlag "MSI" = Just MaximizeSeverityIfInvalid
parseMsFlag _ = Nothing

parseFieldLink :: String -> FieldLink
parseFieldLink "" = NoLink
parseFieldLink ('@' : addr) = HardwareAddr addr
parseFieldLink s | Just n <- maybeRead s = Constant n
parseFieldLink s = FieldLink record field pp ms
  where
    (target : flags) = words s
    (record, field) = case break (== '.') target of
        (record, []) -> (record, "VAL")
        (record, field) -> (record, tail field)
    pp = maybe NoProcessPassive id $ msum $ map parsePpFlag flags
    ms = maybe NonMaximizeSeverity id $ msum $ map parseMsFlag flags

collectLinks p records =
    flip concatMap records $ \r ->
        flip concatMap (r_fields r) $ \(k, [Literal v]) ->
            if p k
                then let [Literal n] = r_name r in [(n, k, v)]
                else []

parseRecordLink :: String -> Maybe RecordName
parseRecordLink = helper . trim
    where
        helper "" = Nothing
        helper s = Just [Literal s]

parseRecordLink' :: String -> RecordLink
parseRecordLink' s = case parseRecordLink s of
    Nothing -> NoRLink
    Just rn -> RecordLink $ substStringToStr rn

outputLinks = collectLinks $ \k -> "LNK" `isPrefixOf` k || k == "OUT"
inputLinks = collectLinks $ \k -> "INP" `isPrefixOf` k || k == "DOL"

recordNameToStr :: RecordName -> String
recordNameToStr = substStringToStr

findRecord :: RecordName -> Database -> Maybe Record
findRecord rn = find (\r -> r_name r == rn || rn `elem` r_aliases r)
