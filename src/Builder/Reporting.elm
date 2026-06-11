{- MANUALLY FORMATTED -}
module Builder.Reporting exposing
  ( Style
  , silent
  , json
  , terminal
  --
  , attempt
  , attemptWithStyle
  --
  , Key
  , report
  , ignorer
  , ask
  --
  , DKey
  , DMsg(..)
  , DState
  , initialDState
  , trackDetails
  --
  , BKey
  , BMsg(..)
  , trackBuild
  --
  , reportGenerate
  )


import Builder.Reporting.Exit as Exit
import Builder.Reporting.Exit.Help as Help
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Encode as Encode
import Compiler.Reporting.Doc as D exposing (d, da)
import Extra.Platform as Platform
import Extra.Platform.Exit as PExit
import Extra.Platform.Handle as Handle
import Extra.System.IO as IO exposing (IO)
import Extra.System.Path as Path exposing (FilePath)
import Extra.Type.Either exposing (Either(..))
import Extra.Type.Lens exposing (Lens)
import Extra.Type.List as MList



-- STATE AND IO


type alias GlobalState b c d e =
  Platform.GlobalState b c d e


type alias IO b c d e v =
  Platform.IO b c d e v



-- STYLE


type Style
  = Silent
  | Json
  | Terminal {- MVar.new -}


silent : Style
silent =
  Silent


json : Style
json =
  Json


terminal : IO b c d e Style
terminal =
  IO.return Terminal



-- ATTEMPT


attempt : (x -> Help.Report) -> IO b c d e (Either x v) -> IO b c d e v
attempt toReport work =
  IO.bind (IO.catch reportExceptionsNicely work) <| \result ->
  case result of
    Right a ->
      IO.return a

    Left x ->
      IO.bind (Exit.toStderr (toReport x)) <| \_ ->
      PExit.exitFailure


attemptWithStyle : Style -> (x -> Help.Report) -> IO b c d e (Either x v) -> IO b c d e v
attemptWithStyle style toReport work =
  IO.bind (IO.catch reportExceptionsNicely work) <| \result ->
  case result of
    Right a ->
      IO.return a

    Left x ->
      case style of
        Silent ->
          PExit.exitFailure

        Json ->
          IO.bind (Handle.hPutStr Handle.stderr (Encode.encodeUgly (Exit.toJson (toReport x)))) <| \_ ->
          PExit.exitFailure

        Terminal ->
          IO.bind (Exit.toStderr (toReport x)) <| \_ ->
          PExit.exitFailure



-- MARKS


goodMark : D.Doc
goodMark =
  D.greenS <| if isWindows then "+" else "●"


badMark : D.Doc
badMark =
  D.redS <| if isWindows then "X" else "✗"


isWindows : Bool
isWindows =
  False



-- KEY


type Key s msg = Key (msg -> IO.IO s ())


report : Key s msg -> msg -> IO.IO s ()
report (Key send) msg =
  send msg


ignorer : Key s msg
ignorer =
  Key (\_ -> IO.return ())



-- ASK


ask : D.Doc -> IO b c d e Bool
ask doc =
  askHelp doc


askHelp : D.Doc -> IO b c d e Bool
askHelp prompt =
  IO.bind (IO.catch handleReadError (D.toAnsi Handle.stdIn prompt)) <| \input ->
    case input of
      ""  -> IO.return True
      "Y" -> IO.return True
      "y" -> IO.return True
      "n" -> IO.return False
      _   ->
        askHelp (D.fromChars "Must type 'y' for yes or 'n' for no: ")


handleReadError : String -> IO b c d e String
handleReadError err =
  if err == "CTRL-C" then
    IO.bind (Platform.consoleWrite "^C\n") <| \_ ->
    Platform.exitFailure 130
  else if err == "CTRL-D" then
    IO.bind (Platform.consoleWrite "^D") <| \_ ->
    IO.error "<stdin>: end of file"
  else
    IO.error ("<stdin>: " ++ err)



-- DETAILS


type alias DKey s = Key s DMsg


trackDetails : Lens (GlobalState b c d e) DState -> Style -> (DKey (GlobalState b c d e) -> IO b c d e a) -> IO b c d e a
trackDetails lensDState style callback =
  case style of
    Silent ->
      callback (Key (\_ -> IO.return ()))

    Json ->
      callback (Key (\_ -> IO.return ()))

    Terminal ->
      IO.bind (IO.putLens lensDState initialDState) <| \_ ->
      let dkey = Key (\msg -> detailsLoop lensDState (Just msg)) in
      IO.bind (callback dkey) <| \answer ->
      IO.bind (detailsLoop lensDState Nothing) <| \_ ->
      IO.return answer


detailsLoop : Lens (GlobalState b c d e) DState -> Maybe DMsg -> IO b c d e ()
detailsLoop lensDState msg =
  IO.bind (IO.getLens lensDState) <| \(DState total _ _ _ _ built _ as state) ->
  case msg of
    Just dmsg ->
      IO.bind (detailsStep dmsg state) (IO.putLens lensDState)

    Nothing ->
      Platform.consoleWrite <| clear (toBuildProgress total total) <|
        if built == total
        then "Dependencies ready!\n"
        else "Dependency problem!\n"


type DState =
  DState
    {- total -} Int
    {- cached -} Int
    {- requested -} Int
    {- received -} Int
    {- failed -} Int
    {- built -} Int
    {- broken -} Int


initialDState : DState
initialDState =
  DState 0 0 0 0 0 0 0


type DMsg
  = DStart Int
  | DCached
  | DRequested
  | DReceived Pkg.Name V.Version
  | DFailed Pkg.Name V.Version
  | DBuilt
  | DBroken


detailsStep : DMsg -> DState -> IO b c d e DState
detailsStep msg (DState total cached rqst rcvd failed built broken) =
  case msg of
    DStart numDependencies ->
      IO.return (DState numDependencies 0 0 0 0 0 0)

    DCached ->
      putTransition (DState total (cached + 1) rqst rcvd failed built broken)

    DRequested ->
      IO.bind (IO.when (rqst == 0) (\() -> Platform.consoleWrite "Starting downloads...\n\n")) <| \_ ->
      IO.return (DState total cached (rqst + 1) rcvd failed built broken)

    DReceived pkg vsn ->
      IO.bind (putDownload goodMark pkg vsn) <| \_ ->
      putTransition (DState total cached rqst (rcvd + 1) failed built broken)

    DFailed pkg vsn ->
      IO.bind (putDownload badMark pkg vsn) <| \_ ->
      putTransition (DState total cached rqst rcvd (failed + 1) built broken)

    DBuilt ->
      putBuilt (DState total cached rqst rcvd failed (built + 1) broken)

    DBroken ->
      putBuilt (DState total cached rqst rcvd failed built (broken + 1))


putDownload : D.Doc -> Pkg.Name -> V.Version -> IO b c d e ()
putDownload mark pkg vsn =
  Help.toStdout <| D.indent 2 <|
    D.hcat [ D.hsep [ mark
    , D.fromPackage pkg
    , D.fromVersion vsn ]
    , d"\n" ]


putTransition : DState -> IO b c d e DState
putTransition (DState total cached _ rcvd failed built broken as state) =
  if cached + rcvd + failed < total then
    IO.return state

  else
    let begin = if rcvd + failed == 0 then "\r" else "\n" in
    IO.bind (putStrFlush (begin ++ toBuildProgress (built + broken + failed) total)) <| \_ ->
    IO.return state


putBuilt : DState -> IO b c d e DState
putBuilt (DState total cached _ rcvd failed built broken as state) =
  IO.bind (IO.when (total == cached + rcvd + failed) <| \() ->
            putStrFlush <| "\r"  ++ toBuildProgress (built + broken + failed) total) <| \_ ->
  IO.return state


toBuildProgress : Int -> Int -> String
toBuildProgress built total =
  "Verifying dependencies (" ++ String.fromInt built ++ "/" ++ String.fromInt total ++ ")"


clear : String -> String -> String
clear before after =
  "\r" ++ String.repeat (String.length before) " " ++ "\r" ++ after



-- BUILD


type alias BKey s = Key s BMsg

type alias BResult a = Either Exit.BuildProblem a


trackBuild : Lens (GlobalState b c d e) Int -> Style -> (BKey (GlobalState b c d e) -> IO b c d e (BResult a)) -> IO b c d e (BResult a)
trackBuild lensDone style callback =
  case style of
    Silent ->
      callback (Key (\_ -> IO.return ()))

    Json ->
      callback (Key (\_ -> IO.return ()))

    Terminal ->
      IO.bind (IO.putLens lensDone 0) <| \_ ->
      IO.bind (putStrFlush "Compiling ...") <| \_ ->
      let bkey = Key (\msg -> buildLoop lensDone (Left msg)) in
      IO.bind (callback bkey) <| \result ->
      IO.bind (buildLoop lensDone (Right result)) <| \_ ->
      IO.return result


type BMsg
  = BDone


buildLoop : Lens (GlobalState b c d e) Int -> Either BMsg (BResult a) -> IO b c d e ()
buildLoop lensDone msg =
  IO.bind (IO.getLens lensDone) <| \done ->
  case msg of
    Left BDone ->
      let done1 = done + 1 in
      IO.bind (putStrFlush <| "\rCompiling (" ++ String.fromInt done1 ++ ")") <| \_ ->
      IO.putLens lensDone done1

    Right result ->
      let
        message = toFinalMessage done result
        width = 12 + String.length (String.fromInt done)
      in
      Platform.consoleWrite <|
        if String.length message < width
        then "\r" ++ String.repeat width " " ++ "\r" ++ message ++ "\n"
        else "\r" ++ message ++ "\n"


toFinalMessage : Int -> BResult a -> String
toFinalMessage done result =
  case result of
    Right _ ->
      case done of
        0 -> "Success!"
        1 -> "Success! Compiled 1 module."
        n -> "Success! Compiled " ++ String.fromInt n ++ " modules."

    Left problem ->
      case problem of
        Exit.BuildBadModules _ _ [] ->
          "Detected problems in 1 module."

        Exit.BuildBadModules _ _ (_::ps) ->
          "Detected problems in " ++ String.fromInt (2 + MList.length ps) ++ " modules."

        Exit.BuildProjectProblem _ ->
          "Detected a problem."



-- GENERATE


reportGenerate : Style -> NE.TList ModuleName.Raw -> FilePath -> IO b c d e ()
reportGenerate style names output =
  case style of
    Silent ->
      IO.return ()

    Json ->
      IO.return ()

    Terminal ->
      let cnames = NE.fmap ModuleName.toChars names in
      Platform.consoleWrite ("\n" ++ toGenDiagram cnames (Path.toString output))


toGenDiagram : NE.TList String -> String -> String
toGenDiagram (NE.CList name names) output =
  let
    width = 3 + MList.foldr (max << String.length) (String.length name) names
  in
  case names of
    [] ->
      toGenLine width name ("> " ++ (output ++ "\n\n"))

    _::_ ->
      String.concat <| MList.map (\line -> line ++ "\n") <|
        toGenLine width name (vtop ++ hbar ++ hbar ++ "> " ++ output)
        :: MList.reverse (MList.zipWith (toGenLine width) (MList.reverse names) (vbottom :: MList.replicate (MList.length names - 1) vmiddle))


toGenLine : Int -> String -> String -> String
toGenLine width name end =
  "    " ++ name ++ " " ++ String.repeat (width - String.length name) hbar ++ end


hbar : String
hbar = if isWindows then "-" else "─"

vtop : String
vtop = if isWindows then "+" else "┬"

vmiddle : String
vmiddle = if isWindows then "+" else "┤"

vbottom : String
vbottom = if isWindows then "+" else "┘\n"


--


putStrFlush : String -> IO b c d e ()
putStrFlush str =
  Handle.hPutStr Handle.stdout str



-- REPORT EXCEPTIONS NICELY


reportExceptionsNicely : String -> IO b c d e a
reportExceptionsNicely e =
  IO.bind (putException e) <| \_ -> IO.bind (Platform.consoleError ("elm: " ++ e ++ "\n")) <| \_ -> PExit.exitFailure


putException : String -> IO b c d e ()
putException e =
  IO.bind (Handle.hPutStr Handle.stderr "\n") <| \_ ->
  Help.toStderr <| D.stack <|
    [ D.dullyellowS "-- ERROR -----------------------------------------------------------------------"
    , D.reflow <|
        "I ran into something that bypassed the normal error reporting process!"
        ++ " I extracted whatever information I could from the internal error:"
    , D.vcat <| MList.map (\line -> da[D.redS ">", d"   ", D.fromChars line]) (String.lines e)
    , D.reflow <|
        "These errors are usually pretty confusing, so start by asking around on one of"
        ++ " forums listed at https://elm-lang.org/community to see if anyone can get you"
        ++ " unstuck quickly."
    , D.dullyellowS "-- REQUEST ---------------------------------------------------------------------"
    , D.reflow <|
        "If you are feeling up to it, please try to get your code down to the smallest"
        ++ " version that still triggers this message. Ideally in a single Main.elm and"
        ++ " elm.json file."
    , D.reflow <|
        "From there open a NEW issue at https://github.com/elm/compiler/issues with"
        ++ " your reduced example pasted in directly. (Not a link to a repo or gist!) Do not"
        ++ " worry about if someone else saw something similar. More examples is better!"
    , D.reflow <|
        "This kind of error is usually tied up in larger architectural choices that are"
        ++ " hard to change, so even when we have a couple good examples, it can take some"
        ++ " time to resolve in a solid way."
    ]
