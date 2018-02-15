module CycleCheck where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S

import Parser
import Record


isDataLinkField f =
    "DOL" `isPrefixOf` f ||
    "LNK" `isPrefixOf` f ||
    "IN" `isPrefixOf` f ||
    "OUT" `isPrefixOf` f

getDataLinkEdge :: FieldLink -> Maybe Ident
getDataLinkEdge (FieldLink r f ProcessPassive ms) = Just r
getDataLinkEdge (FieldLink r "PROC" pp ms) = Just r
getDataLinkEdge _ = Nothing

getFieldEdge :: String -> String -> Maybe Ident
getFieldEdge name val
  | isDataLinkField name = getDataLinkEdge $ parseFieldLink val
getFieldEdge "FLNK" val = Just val
getFieldEdge _ _ = Nothing

getRecordEdges :: Record -> [(Ident, Ident)]
getRecordEdges r = map (\t -> (name, t)) $
    mapMaybe (\(n, [Literal v]) -> getFieldEdge n v) $ r_fields r
  where [Literal name] = r_name r

getEdges :: Database -> [(Ident, Ident)]
getEdges db = concatMap getRecordEdges db


findCyclesInEdges :: [(Ident, Ident)] -> [[Ident]]
findCyclesInEdges edges = m3
  where
    edgeMap = M.fromListWith (++) [(s, [t]) | (s,t) <- map idtrace edges]

    go :: [Ident] -> Ident -> StateT (S.Set Ident) (Writer [[Ident]]) ()
    go stack rn = do
        when (rn `elem` stack) $
            tell [rn : takeWhile (/= rn) stack]
            
        let stack' = rn : stack
        seen <- gets $ S.member rn
        when (not seen) $ do
            modify $ S.insert rn
            mapM_ (go stack') (fromMaybe [] $ M.lookup rn edgeMap)

    m1 = mapM_ (go []) $ M.keys edgeMap
    m2 = evalStateT m1 S.empty
    m3 = execWriter m2

findCycles :: Database -> [[Ident]]
findCycles db = dedupCycles $ findCyclesInEdges $ getEdges db

dedupCycles :: [[Ident]] -> [[Ident]]
dedupCycles cs = map snd $ M.toList $ M.fromList [(S.fromList c, c) | c <- cs]
