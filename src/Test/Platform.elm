module Test.Platform exposing
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
import Extra.System.IO as IO
import Extra.System.Path exposing (FilePath)
import Extra.Type.List exposing (TList)
import Global
import Time



-- STATE


type alias Flags =
    ()


type alias GlobalState b c d e =
    Global.State LocalState b c d e


type alias LocalState =
    ()


toGlobalState : Flags -> b -> c -> d -> e -> GlobalState b c d e
toGlobalState flags b c d e =
    Global.State (initialLocalState flags) b c d e


initialLocalState : Flags -> LocalState
initialLocalState flags =
    always () flags



-- IO


type alias IO b c d e v =
    IO.IO (GlobalState b c d e) v



-- CONSOLE


consoleRead : String -> String -> IO b c d e String
consoleRead prompt initial =
    (\_ -> Debug.todo "dummy consoleRead") ( prompt, initial )


consoleWrite : String -> IO b c d e ()
consoleWrite str =
    (\_ -> Debug.todo "dummy consoleWrite") str


consoleError : String -> IO b c d e ()
consoleError str =
    (\_ -> Debug.todo "dummy consoleError") str



-- FILE


createDirectoryIfMissing : FilePath -> IO b c d e ()
createDirectoryIfMissing filePath =
    (\_ -> Debug.todo "dummy createDirectoryIfMissing") filePath


doesDirectoryExist : FilePath -> IO b c d e Bool
doesDirectoryExist filePath =
    (\_ -> Debug.todo "dummy doesDirectoryExist") filePath


doesFileExist : FilePath -> IO b c d e Bool
doesFileExist filePath =
    (\_ -> Debug.todo "dummy doesFileExist") filePath


getCurrentDirectory : IO b c d e FilePath
getCurrentDirectory =
    Debug.todo "dummy getCurrentDirectory"


getElmHome : IO b c d e FilePath
getElmHome =
    Debug.todo "dummy getElmHome"


getModificationTime : FilePath -> IO b c d e Time.Posix
getModificationTime filePath =
    (\_ -> Debug.todo "dummy getModificationTime") filePath


makeAbsolute : FilePath -> IO b c d e FilePath
makeAbsolute filePath =
    (\_ -> Debug.todo "dummy makeAbsolute") filePath


readFile : FilePath -> IO b c d e (Maybe Bytes)
readFile filePath =
    (\_ -> Debug.todo "dummy readFile") filePath


removeFile : FilePath -> IO b c d e ()
removeFile filePath =
    (\_ -> Debug.todo "dummy removeFile") filePath


writeFile : FilePath -> Bytes -> IO b c d e ()
writeFile filePath contents =
    (\_ -> Debug.todo "dummy writeFile") ( filePath, contents )



-- HTTP


httpPrefix : IO b c d e (Maybe String)
httpPrefix =
    Debug.todo "dummy httpPrefix"



-- JAVASCRIPT


getInterpreter : Maybe FilePath -> IO b c d e (String -> IO b c d e Bool)
getInterpreter maybeFilePath =
    (\_ -> Debug.todo "dummy getInterpreter") maybeFilePath



-- PROCESS


getArgs : IO b c d e (TList String)
getArgs =
    Debug.todo "dummy getArgs"


stdOutIsTty : IO b c d e Bool
stdOutIsTty =
    Debug.todo "dummy stdOutIsTty"


stdErrIsTty : IO b c d e Bool
stdErrIsTty =
    Debug.todo "dummy stdErrIsTty"


exitSuccess : IO b c d e v
exitSuccess =
    Debug.todo "dummy exitSuccess"


exitFailure : Int -> IO b c d e v
exitFailure code =
    (\_ -> Debug.todo "dummy exitFailure") code
