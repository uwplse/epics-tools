module CmdGen(genCmdFile) where

import Control.Monad
import System.Random
import qualified Data.Map as M
import Data.Maybe(isJust)

import Record
import Parser (substStringToStr)
import FieldInfo

genCmdFile :: Integer -> Database -> IO String
genCmdFile seed db = do
  setStdGen $ mkStdGen (fromIntegral seed)
  i <- mapM writeRandomInputs db
  tr <- concat `liftM` replicateM (length db) (processRandomRecord db)
  let o = concatMap getOutput db
  return $ concat i ++ tr ++ o

inputs = map (:"") ['A'..'B']

hasField r f =
  case M.lookup (substStringToStr $ r_type r) fieldtypes of
    Just m -> isJust (M.lookup f m)
    _ -> False

writeRandomInputs record =
  case M.lookup (substStringToStr $ r_type record) fieldtypes of
    Just m -> do
      strs <- mapM (uncurry $ writeRandomInput record) (M.toList m)
      return $ concat strs
    _ -> return ""

writeRandomInput r f ty | f `elem` inputs = case ty of
    DOUBLE -> goFloat
    FLOAT -> goFloat
    LONG -> goInt
    ULONG -> goInt
  where
    goFloat = do
        val <- getStdRandom (randomR (0, 100)) :: IO Integer
        let val' = fromIntegral val :: Double
        return $ "dbpf(\"" ++ recordNameToStr (r_name r) ++ "." ++ f ++ "\", \"" ++ show val' ++ "\")\n"
    goInt = do
        val <- getStdRandom (randomR (0, 100)) :: IO Integer
        return $ "dbpf(\"" ++ recordNameToStr (r_name r) ++ "." ++ f ++ "\", \"" ++ show val ++ "\")\n"
writeRandomInput r f ty = return ""

processRandomRecord db = do
  i <- getStdRandom (randomR (0, length db - 1))
  let record = db !! i
  return $ "dbtr(\"" ++ recordNameToStr (r_name record) ++ "\")\n"

getOutput record
  | (substStringToStr $ r_type record) `elem` ["calc", "calcout"] = "dbgf(\"" ++ recordNameToStr (r_name record) ++ ".VAL\")\n"
  | (substStringToStr $ r_type record) `elem` ["acalcout"] = "dbgf(\"" ++ recordNameToStr (r_name record) ++ ".AVAL\")\n"
  | otherwise = ""
