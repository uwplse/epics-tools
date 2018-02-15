module CheckAsyn
where

import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Exit

import Iocsh
import Parser
import Record

-- Inspect st.cmd and the database to check that index `arrIndex` of async
-- record `recordName` corresponds to relay `relayNumber` in the PLC.
checkRelayWrites :: String -> Int -> Int -> [Command] -> Database -> IO ()
checkRelayWrites recordName arrIndex relayNumber cmds db = do
    -- First, find the asyn device name by inspecting the named record.
    let r = fromMaybe (error $ "no record " ++ show recordName ++ " in database") $
                findRecord [Literal recordName] db
    if substStringToStr (r_type r) == "waveform" then return () else error "expected a waveform record"
    let [Literal hwAddr] = snd $ fromMaybe (error "record is missing INP field") $
                find (\(n,v) -> n == "INP") $ r_fields r

    let [hwAddrA, hwAddrB] = words hwAddr
    if "@asyn(" `isPrefixOf` hwAddrA && "0)MODBUS_DATA" == hwAddrB then
        return ()
    else
        error $ "expected INP to match `@asyn(<ASYN_NAME> 0)MODBUS_DATA` " ++
            "(instead got " ++ show hwAddr ++ ")"
    let asynAddr = drop (length "@asyn(") hwAddrA

    -- Now find the definition of this asyn device name.
    let FuncCmd _ confArgs@[_, asynConfigName, slaveAddr, modbusFunction,
                modbusStartIndex, modbusLength, modbusDataType, pollMsec, plcType] =
            fromMaybe (error $ "couldn't find asyn config for " ++ show asynAddr) $
                flip find cmds $ \c -> case c of
                    FuncCmd "drvModbusAsynConfigure" (addr : _) | addr == asynAddr -> True
                    _ -> False

    -- These are magic numbers based on what I saw in the st.cmd.  The asyn
    -- manual explains what these mean.
    if modbusFunction == "15" &&
            modbusDataType == "0" &&
            pollMsec == "1" &&
            plcType == "Modicon" then
        return ()
    else
        error $ "unexpected arguments for drvModbusAsynConfigure: " ++ show confArgs

    -- This is the off-by-one weirdness that Jon explained to us.
    let asynBase = read modbusStartIndex + 1
    let actualRelay = asynBase + arrIndex
    if relayNumber == actualRelay then
        return ()
    else do
        putStrLn $ "Record " ++ recordName ++ "[" ++ show arrIndex ++ "] matches to wrong PLC relay"
        putStrLn $ "  Expected relay: " ++ show relayNumber
        putStrLn $ "  Actual relay: " ++ show actualRelay ++
            " (" ++ show asynBase ++ " + " ++ show arrIndex ++ ")"
        exitWith $ ExitFailure 1

    let asynLen = read modbusLength
    if arrIndex < asynLen then
        return ()
    else do
        putStrLn $ "Record " ++ recordName ++ "[" ++ show arrIndex ++ "] has out-of-bounds index"
        putStrLn $ "  Index: " ++ show arrIndex
        putStrLn $ "  Limit: " ++ show asynLen
        exitWith $ ExitFailure 1

    return ()
