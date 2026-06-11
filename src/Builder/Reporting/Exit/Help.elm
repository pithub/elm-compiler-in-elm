{- MANUALLY FORMATTED -}
module Builder.Reporting.Exit.Help exposing
  ( Report
  , report
  , docReport
  , jsonReport
  , compilerReport
  , reportToDoc
  , reportToJson
  , reportToClient
  , toStdout
  , toStderr
  )


import Compiler.Json.Encode as E
import Compiler.Reporting.Doc as D exposing (d)
import Compiler.Reporting.Error as Error
import Elm.Error as Client
import Extra.Platform as Platform
import Extra.Platform.Handle as Handle exposing (Handle)
import Extra.System.IO as IO
import Extra.System.Path as Path exposing (FilePath)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe



-- STATE AND IO


type alias GlobalState b c d e =
  Platform.GlobalState b c d e


type alias IO b c d e v =
  Platform.IO b c d e v



-- REPORT


type Report
  = CompilerReport FilePath Error.Module (TList Error.Module)
  | Report
      {- title -} String
      {- path -} (Maybe FilePath)
      {- message -} D.Doc


report : String -> Maybe FilePath -> String -> TList D.Doc -> Report
report title path startString others =
  Report title path <| D.stack (D.reflow startString::others)


docReport : String -> Maybe FilePath -> D.Doc -> TList D.Doc -> Report
docReport title path startDoc others =
  Report title path <| D.stack (startDoc::others)


jsonReport : String -> Maybe FilePath -> D.Doc -> Report
jsonReport =
  Report


compilerReport : FilePath -> Error.Module -> TList Error.Module -> Report
compilerReport =
  CompilerReport



-- TO DOC


reportToDoc : Report -> D.Doc
reportToDoc report_ =
  case report_ of
    CompilerReport root e es ->
      Error.toDoc root e es

    Report title maybePath message ->
      let
        makeDashes n =
          String.repeat (max 1 (80 - n)) "-"

        errorBarEnd =
          case maybePath of
            Nothing ->
              makeDashes (4 + String.length title)

            Just path ->
              makeDashes (5 + String.length title + String.length (Path.toString path)) ++ " " ++ (Path.toString path)

        errorBar =
          D.dullcyan <|
            D.hsep [d"--", D.fromChars title, D.fromChars errorBarEnd ]
      in
        D.stack [errorBar, message, d""]



-- TO JSON


reportToJson : Report -> E.Value
reportToJson report_ =
  case report_ of
    CompilerReport _ e es ->
      E.object
        [ ("type", E.chars "compile-errors")
        , ("errors", E.list Error.toJson (e::es))
        ]

    Report title maybePath message ->
      E.object
        [ ("type", E.chars "error")
        , ("path", MMaybe.maybe E.null E.path maybePath)
        , ("title", E.chars title)
        , ("message", D.encode message)
        ]



-- TO CLIENT


reportToClient : Report -> Client.Error
reportToClient report_ =
  case report_ of
    CompilerReport _ e es ->
      Client.ModuleProblems <| MList.map Error.toClient (e::es)

    Report title maybePath message ->
      Client.GeneralProblem
        { path = Maybe.map Path.toString maybePath
        , title = title
        , message = D.toClient message
        }



-- OUTPUT


toString : D.Doc -> String
toString =
  D.toString


toStdout : D.Doc -> IO b c d e ()
toStdout doc =
  toHandle Handle.stdout doc


toStderr : D.Doc -> IO b c d e ()
toStderr doc =
  toHandle Handle.stderr doc


toHandle : Handle (GlobalState b c d e) () -> D.Doc -> IO b c d e ()
toHandle handle doc =
  IO.bind (Handle.hIsTerminalDevice handle) <| \isTerminal ->
  if isTerminal
    then D.toAnsi handle doc
    else Handle.hPutStr handle (toString doc)
