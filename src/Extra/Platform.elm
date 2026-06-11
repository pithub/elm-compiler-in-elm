module Extra.Platform exposing
    ( Flags
    , GlobalState
    , IO
    , consoleError
    , consoleRead
    , consoleWrite
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , exitFailure
    , exitSuccess
    , getArgs
    , getCurrentDirectory
    , getElmHome
    , getInterpreter
    , getModificationTime
    , httpPrefix
    , makeAbsolute
    , readFile
    , removeFile
    , stdErrIsTty
    , stdOutIsTty
    , toGlobalState
    , writeFile
    )

import Bytes exposing (Bytes)
import Extra.System.Path exposing (FilePath)
import Extra.Type.List exposing (TList)
import Test.Platform as Internal
import Time



-- STATE


type alias Flags =
    Internal.Flags


type alias GlobalState b c d e =
    Internal.GlobalState b c d e


toGlobalState : Flags -> b -> c -> d -> e -> GlobalState b c d e
toGlobalState =
    Internal.toGlobalState



-- IO


type alias IO b c d e v =
    Internal.IO b c d e v



-- CONSOLE


consoleRead : String -> String -> IO b c d e String
consoleRead prompt initial =
    Internal.consoleRead prompt initial


consoleWrite : String -> IO b c d e ()
consoleWrite str =
    Internal.consoleWrite str


consoleError : String -> IO b c d e ()
consoleError str =
    Internal.consoleError str



-- FILE


createDirectoryIfMissing : FilePath -> IO b c d e ()
createDirectoryIfMissing filePath =
    Internal.createDirectoryIfMissing filePath


doesDirectoryExist : FilePath -> IO b c d e Bool
doesDirectoryExist filePath =
    Internal.doesDirectoryExist filePath


doesFileExist : FilePath -> IO b c d e Bool
doesFileExist filePath =
    Internal.doesFileExist filePath


getCurrentDirectory : IO b c d e FilePath
getCurrentDirectory =
    Internal.getCurrentDirectory


getElmHome : IO b c d e FilePath
getElmHome =
    Internal.getElmHome


getModificationTime : FilePath -> IO b c d e Time.Posix
getModificationTime filePath =
    Internal.getModificationTime filePath


makeAbsolute : FilePath -> IO b c d e FilePath
makeAbsolute filePath =
    Internal.makeAbsolute filePath


readFile : FilePath -> IO b c d e (Maybe Bytes)
readFile filePath =
    Internal.readFile filePath


removeFile : FilePath -> IO b c d e ()
removeFile filePath =
    Internal.removeFile filePath


writeFile : FilePath -> Bytes -> IO b c d e ()
writeFile filePath contents =
    Internal.writeFile filePath contents



-- HTTP


httpPrefix : IO b c d e (Maybe String)
httpPrefix =
    Internal.httpPrefix



-- JAVASCRIPT


getInterpreter : Maybe FilePath -> IO b c d e (String -> IO b c d e Bool)
getInterpreter maybeFilePath =
    Internal.getInterpreter maybeFilePath



-- PROCESS


getArgs : IO b c d e (TList String)
getArgs =
    Internal.getArgs


stdOutIsTty : IO b c d e Bool
stdOutIsTty =
    Internal.stdOutIsTty


stdErrIsTty : IO b c d e Bool
stdErrIsTty =
    Internal.stdErrIsTty


exitSuccess : IO b c d e v
exitSuccess =
    Internal.exitSuccess


exitFailure : Int -> IO b c d e v
exitFailure code =
    Internal.exitFailure code
