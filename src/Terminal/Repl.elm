{- MANUALLY FORMATTED -}
module Terminal.Repl exposing
  ( Flags(..)
  , run
  --
  , Lines(..)
  , CategorizedInput
  --
  , State
  --
  , Env(..)
  , Outcome
  )


import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.Generate as Generate
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.Name as N
import Compiler.Elm.Constraint as C
import Compiler.Elm.Licenses as Licenses
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Parse.Declaration as PD
import Compiler.Parse.Expression as PE
import Compiler.Parse.Module as PM
import Compiler.Parse.Primitives as P exposing (Row, Col)
import Compiler.Parse.Space as PS
import Compiler.Parse.Type as PT
import Compiler.Parse.Variable as PV
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (d)
import Compiler.Reporting.Error.Syntax as ES
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Extra.Platform as Platform
import Extra.Platform.Handle as Handle
import Extra.System.IO as IO
import Extra.System.Path as Path exposing (FilePath)
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Unicode as UChar



-- STATE AND IO


type alias GlobalState h =
  Generate.GlobalState h


type alias IO h v =
  IO.IO (GlobalState h) v



-- RUN


type Flags =
  Flags
    {- maybeInterpreter -} (Maybe FilePath)
    {- noColors -} Bool


run : () -> Flags -> IO h ()
run _ flags =
  IO.bind printWelcomeMessage <| \_ ->
  IO.bind (initEnv flags) <| \env ->
  loop env initialState



-- WELCOME


printWelcomeMessage : IO h ()
printWelcomeMessage =
  let
    vsn = V.toChars V.compiler
    title = D.hsep [d"Elm", D.fromChars vsn]
    dashes = String.repeat (70 - String.length vsn) "-"
  in
  D.toAnsi Handle.stdout <|
    D.vcat
      [ D.hsep [D.blackS "----", D.dullcyan title, D.blackS dashes]
      , D.blackS <| "Say :help for help and :exit to exit! More at " ++ D.makeLink "repl"
      , D.blackS "--------------------------------------------------------------------------------"
      , D.empty
      , d""
      ]



-- ENV


type Env h =
  Env
    {- root -} FilePath
    {- interpreter -} (String -> IO h Bool)
    {- ansi -} Bool


initEnv : Flags -> IO h (Env h)
initEnv (Flags maybeAlternateInterpreter noColors) =
  IO.bind getRoot <| \root ->
  IO.bind (Platform.getInterpreter maybeAlternateInterpreter) <| \interpreter ->
  IO.return <| Env root interpreter (not noColors)



-- LOOP


type Outcome
  = Loop State
  | End


loop : Env h -> State -> IO h ()
loop env state =
  IO.bind (IO.catch handleReadError read) <| \input ->
  IO.bind (eval env state input) <| \outcome ->
  case outcome of
    Loop state_ ->
      loop env state_

    End ->
      IO.return ()


handleReadError : String -> IO h Input
handleReadError err =
  if err == "CTRL-C" then
    IO.bind (Platform.consoleWrite "\n") <| \_ ->
    IO.return Skip
  else if err == "CTRL-D" then
    IO.bind (Platform.consoleWrite "\n") <| \_ ->
    IO.return Exit
  else
    IO.bind (Platform.consoleError (err ++ "\n")) <| \_ ->
    Platform.exitFailure 1



-- READ


type Input
  = Import ModuleName.Raw String
  | Type N.Name String
  | Port
  | Decl N.Name String
  | Expr String
  --
  | Reset
  | Exit
  | Skip
  | Help (Maybe String)


read : IO h Input
read =
  IO.bind (Platform.consoleRead "> " "") <| \line ->
  let
    lines = Lines (stripLegacyBackslash line) []
  in
  case categorize lines of
    Done input -> IO.return input
    Continue p -> readMore lines p


readMore : Lines -> Prefill -> IO h Input
readMore previousLines prefill =
  IO.bind (Platform.consoleRead "| " (renderPrefill prefill)) <| \input ->
  let
    lines = addLine (stripLegacyBackslash input) previousLines
  in
  case categorize lines of
    Done input_ -> IO.return input_
    Continue p -> readMore lines p


-- For compatibility with 0.19.0 such that readers of "Programming Elm" by @jfairbank
-- can get through the REPL section successfully.
--
-- TODO: remove stripLegacyBackslash in next MAJOR release
--
stripLegacyBackslash : String -> String
stripLegacyBackslash chars =
  if String.endsWith "\\" chars then
    String.dropRight 1 chars
  else
    chars


type Prefill
  = Indent
  | DefStart N.Name


renderPrefill : Prefill -> String
renderPrefill lineStart =
  case lineStart of
    Indent ->
      "  "

    DefStart name ->
      name ++ " "



-- LINES


type Lines =
  Lines
    {- prevLine -} String
    {- revLines -} (TList String)


addLine : String -> Lines -> Lines
addLine line (Lines x xs) =
  Lines line (x::xs)


isBlank : Lines -> Bool
isBlank (Lines prev rev) =
  MList.null rev && String.all ((==)' ') prev


isSingleLine : Lines -> Bool
isSingleLine (Lines _ rev) =
  MList.null rev


endsWithBlankLine : Lines -> Bool
endsWithBlankLine (Lines prev _) =
  String.all ((==)' ') prev


linesToByteString : Lines -> String
linesToByteString (Lines prev rev) =
  String.join "\n" (MList.reverse (prev::rev)) ++ "\n"


getFirstLine : Lines -> String
getFirstLine (Lines x xs) =
  case xs of
    []   -> x
    y::ys -> getFirstLine (Lines y ys)



-- CATEGORIZE INPUT


type CategorizedInput
  = Done Input
  | Continue Prefill


categorize : Lines -> CategorizedInput
categorize lines =
  if isBlank lines                         then Done Skip
  else if startsWithColon lines            then Done (toCommand lines)
  else if startsWithKeyword "import" lines then attemptImport lines
  else                                          attemptDeclOrExpr lines


attemptImport : Lines -> CategorizedInput
attemptImport lines =
  let
    src = linesToByteString lines
    parser = P.specialize (\_ _ _ -> ()) PM.chompImport
  in
  case P.fromByteString parser (\_ _ -> ()) src of
    Right (Src.Import (A.At _ name) _ _) ->
      Done (Import name src)

    Left () ->
      ifFail lines (Import "ERR" src)


ifFail : Lines -> Input -> CategorizedInput
ifFail lines input =
  if endsWithBlankLine lines
  then Done input
  else Continue Indent


ifDone : Lines -> Input -> CategorizedInput
ifDone lines input =
  if isSingleLine lines || endsWithBlankLine lines
  then Done input
  else Continue Indent


attemptDeclOrExpr : Lines -> CategorizedInput
attemptDeclOrExpr lines =
  let
    src = linesToByteString lines
    exprParser = P.specialize (toExprPosition src) PE.expression
    declParser = P.specialize (toDeclPosition src) PD.declaration
  in
  case P.fromByteString declParser Tuple.pair src of
    Right (decl, _) ->
      case decl of
        PD.Value (A.At _ (Src.Value (A.At _ name) _ _ _)) -> ifDone lines (Decl name src)
        PD.Union (A.At _ (Src.Union (A.At _ name) _ _  )) -> ifDone lines (Type name src)
        PD.Alias (A.At _ (Src.Alias (A.At _ name) _ _  )) -> ifDone lines (Type name src)
        PD.Port  _                                        -> Done Port

    Left declPosition ->
      if startsWithKeyword "type" lines then
        ifFail lines (Type "ERR" src)

      else if startsWithKeyword "port" lines then
        Done Port

      else
        case P.fromByteString exprParser Tuple.pair src of
          Right _ ->
            ifDone lines (Expr src)

          Left exprPosition ->
            if exprPosition >= declPosition then
              ifFail lines (Expr src)
            else
              case P.fromByteString annotation (\_ _ -> ()) src of
                Right name -> Continue (DefStart name)
                Left ()    -> ifFail lines (Decl "ERR" src)


startsWithColon : Lines -> Bool
startsWithColon lines =
  case String.uncons <| String.trimLeft (getFirstLine lines) of
    Nothing -> False
    Just (c,_) -> c == ':'


toCommand : Lines -> Input
toCommand lines =
  case String.dropLeft 1 <| String.trimLeft (getFirstLine lines) of
    "reset"       -> Reset
    "exit"        -> Exit
    "quit"        -> Exit
    "help"        -> Help Nothing
    rest          -> Help (List.head <| String.words rest)


startsWithKeyword : String -> Lines -> Bool
startsWithKeyword keyword lines =
  let
    line = getFirstLine lines
  in
  String.startsWith keyword line &&
    case String.uncons <| String.dropLeft (String.length keyword) line of
      Nothing -> True
      Just (c,_) -> not (UChar.isAlphaNum c)


toExprPosition : String -> ES.Expr -> Row -> Col -> (Row, Col)
toExprPosition src expr row col =
  let
    decl = ES.DeclDef N.replValueToPrint (ES.DeclDefBody expr row col) row col
  in
  toDeclPosition src decl row col


toDeclPosition : String -> ES.Decl -> Row -> Col -> (Row, Col)
toDeclPosition src decl r c =
  let
    err = ES.ParseError (ES.Declarations decl r c)
    report = ES.toReport (Code.toSource src) err

    (Report.Report _ (A.Region (A.Position row col) _) _) = report
  in
  (row, col)


annotation : P.Parser () N.Name
annotation =
  let
    err _ _ = ()
    err_ _ _ _ = ()
  in
  P.bind (PV.lower err) <| \name ->
  P.bind (PS.chompAndCheckIndent err_ err) <| \_ ->
  P.bind (P.word1 0x3A {-:-} err) <| \_ ->
  P.bind (PS.chompAndCheckIndent err_ err) <| \_ ->
  P.bind (P.specialize err_ PT.expression) <| \_ ->
  P.bind (PS.checkFreshLine err) <| \_ ->
  P.return name



-- STATE


type State =
  State
    {- imports -} (Map.Map N.Name String)
    {- types -} (Map.Map N.Name String)
    {- decls -} (Map.Map N.Name String)


initialState : State
initialState =
  State Map.empty Map.empty Map.empty



-- EVAL


eval : Env h -> State -> Input -> IO h Outcome
eval env ((State imports types decls) as state) input =
  case input of
    Skip ->
      IO.return (Loop state)

    Exit ->
      IO.return End

    Reset ->
      IO.bindSequence
        [ Platform.consoleWrite "<reset>\n" ]
        (IO.return (Loop initialState))

    Help maybeUnknownCommand ->
      IO.bind (Platform.consoleWrite (toHelpMessage maybeUnknownCommand)) <| \_ ->
      IO.return (Loop state)

    Import name src ->
      let newState = State (Map.insert name src imports) types decls in
      IO.fmap Loop <| attemptEval env state newState OutputNothing

    Type name src ->
      let newState = State imports (Map.insert name src types) decls in
      IO.fmap Loop <| attemptEval env state newState OutputNothing

    Port ->
      IO.bind (Platform.consoleWrite "I cannot handle port declarations.\n") <| \_ ->
      IO.return (Loop state)

    Decl name src ->
      let newState = State imports types (Map.insert name src decls) in
      IO.fmap Loop <| attemptEval env state newState (OutputDecl name)

    Expr src ->
      IO.fmap Loop <| attemptEval env state state (OutputExpr src)



-- ATTEMPT EVAL


type Output
  = OutputNothing
  | OutputDecl N.Name
  | OutputExpr String


attemptEval : Env h -> State -> State -> Output -> IO h State
attemptEval (Env root interpreter ansi) oldState newState output =
  IO.bind
    (Task.run <|
      Task.bind
        (Task.eio Exit.ReplBadDetails <|
          Details.load Reporting.silent root) <| \details ->

      Task.bind
        (Task.eio identity <|
          Build.fromRepl root details (toByteString newState output)) <| \artifacts ->

      MMaybe.traverse Task.pure Task.fmap (Task.mapError Exit.ReplBadGenerate << Generate.repl root details ansi artifacts) (toPrintName output)) <| \result ->

  case result of
    Left exit ->
      IO.bind (Exit.toStderr (Exit.replToReport exit)) <| \_ ->
      IO.return oldState

    Right Nothing ->
      IO.return newState

    Right (Just javascript) ->
      IO.bind (interpreter javascript) <| \exitSuccess ->
      if exitSuccess
        then IO.return newState
        else IO.return oldState



-- TO BYTESTRING


toByteString : State -> Output -> String
toByteString (State imports types decls) output =
  String.concat
    [ "module " ++ N.toBuilder N.replModule ++ " exposing (..)\n"
    , Map.foldr (++) "" imports
    , Map.foldr (++) "" types
    , Map.foldr (++) "" decls
    , outputToBuilder output
    ]


outputToBuilder : Output -> String
outputToBuilder output =
  N.toBuilder N.replValueToPrint ++ " =" ++
  case output of
    OutputNothing ->
      " ()\n"

    OutputDecl _ ->
      " ()\n"

    OutputExpr expr ->
      MList.foldr (\line rest -> "\n  " ++ line ++ rest) "\n" (String.split "\n" expr)



-- TO PRINT NAME


toPrintName : Output -> Maybe N.Name
toPrintName output =
  case output of
    OutputNothing   -> Nothing
    OutputDecl name -> Just name
    OutputExpr _    -> Just N.replValueToPrint



-- HELP MESSAGES


toHelpMessage : Maybe String -> String
toHelpMessage maybeBadCommand =
  case maybeBadCommand of
    Nothing ->
      genericHelpMessage

    Just command ->
      "I do not recognize the :" ++ command ++ " command. " ++ genericHelpMessage


genericHelpMessage : String
genericHelpMessage =
  "Valid commands include:\n"
  ++ "\n"
  ++ "  :exit    Exit the REPL\n"
  ++ "  :help    Show this information\n"
  ++ "  :reset   Clear all previous imports and definitions\n"
  ++ "\n"
  ++ "More info at " ++ D.makeLink "repl" ++ "\n\n"



-- GET ROOT


getRoot : IO h FilePath
getRoot =
  IO.bind Stuff.findRoot <| \maybeRoot ->
  case maybeRoot of
    Just root ->
      IO.return root

    Nothing ->
      IO.bind Stuff.getReplCache <| \cache ->
      let root = Path.addName cache "tmp" in
      IO.bind (Platform.createDirectoryIfMissing (Path.addName root "src")) <| \_ ->
      IO.bind (Outline.write root <| Outline.Pkg <|
        Outline.PkgOutline
          Pkg.dummyName
          Outline.defaultSummary
          Licenses.bsd3
          V.one
          (Outline.ExposedList [])
          defaultDeps
          Map.empty
          C.defaultElm) <| \_ ->

      IO.return root


defaultDeps : Map.Map Pkg.Comparable C.Constraint
defaultDeps =
  Map.fromList
    [ (Pkg.toComparable Pkg.core, C.anything)
    , (Pkg.toComparable Pkg.json, C.anything)
    , (Pkg.toComparable Pkg.html, C.anything)
    ]
