{- MANUALLY FORMATTED -}
module Terminal.Repl exposing
  ( Flags(..)
  , Interpreter
  , InterpreterInput(..)
  , InterpreterResult(..)
  , run
  --
  --, Lines(..)
  --, Input(..)
  --, Prefill(..)
  --, CategorizedInput(..)
  --, categorize
  --
  --, State(..)
  --, Output(..)
  --, toByteString
  --
  , GlobalState
  , LocalState
  , initialLocalState
  --
  , Mode(..)
  , openedModule
  )


import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Generate as Generate
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.Name as N
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Constraint as C
import Compiler.Elm.Licenses as Licenses
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Generate.JavaScript as JS
import Compiler.Parse.Declaration as PD
import Compiler.Parse.Expression as PE
import Compiler.Parse.Module as PM
import Compiler.Parse.Primitives as P exposing (Row, Col)
import Compiler.Parse.Space as PS
import Compiler.Parse.Type as PT
import Compiler.Parse.Variable as PV
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (d)
import Compiler.Reporting.Error as E
import Compiler.Reporting.Error.Import as EI
import Compiler.Reporting.Error.Syntax as ES
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Extra.System.File as SysFile exposing (FilePath)
import Extra.System.IO as IO
import Extra.Type.Either as Either exposing (Either(..))
import Extra.Type.Lens exposing (Lens)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Set as Set
import Global
import Terminal.Command as Command
import Unicode as UChar



-- PUBLIC STATE


type alias GlobalState a g =
  Command.State a g (LocalState a g)


type LocalState a g =
  LocalState
    -- tbd
    ()


initialLocalState : LocalState a g
initialLocalState =
  LocalState
    -- tbd
    ()


lensTbd : Lens (GlobalState a g) ()
lensTbd =
  { getter = \(Global.State _ _ _ _ _ _ _ (LocalState x)) -> x
  , setter = \x (Global.State a b c d e f g _) -> Global.State a b c d e f g (LocalState x)
  }



-- PRIVATE IO


type alias IO a g v =
  IO.IO (GlobalState a g) v



-- RUN


{- NEW: Flags -}
type Flags a g =
  Flags
    {- interpreter -} (Interpreter a g)
    {- mode -} Mode
    {- htmlEnabled -} Bool


{- NEW: Mode -}
type Mode
  = Normal
  | Module String
  | Breakpoint String String String

isBreakpoint : Mode -> Bool
isBreakpoint mode =
  case mode of
    Breakpoint _ _ _ -> True
    _ -> False


type alias Interpreter a g =
  InterpreterInput -> IO a g InterpreterResult

type InterpreterInput
  = InterpretValue String
  | InterpretHtml N.Name String
  | ShowError Exit.Repl

type InterpreterResult
  = InterpreterSuccess
  | InterpreterFailure


run : Flags a g -> IO a g (Either Exit.Repl ())
run flags =
  IO.bind (initEnv flags) <| \envResult ->
    case envResult of
      Left exit ->
        IO.return <| Left exit

      Right env ->
        IO.bind printWelcomeMessage <| \_ ->
        IO.fmap Right <| loop env (initialState env)



-- WELCOME


printWelcomeMessage : IO a g ()
printWelcomeMessage =
  let
    vsn = V.toChars V.compiler
    title = D.hsep [d"Elm", D.fromChars vsn]
    dashes = String.repeat (70 - String.length vsn) "-"
  in
  Command.putDoc <|
    D.vcat
      [ D.hsep [D.blackS "----", D.dullcyan title, D.blackS dashes]
      , D.blackS <| "Say :help for help and :exit to exit! More at " ++ D.makeLink "repl"
      , D.blackS "--------------------------------------------------------------------------------"
      , D.empty
      ]



-- ENV


type Env a g =
  Env
    {- root -} FilePath
    {- interpreter -} (Interpreter a g)
    {- ansi -} Bool
    {- mode -} Mode
    {- modulePrefix -} String
    {- htmlEnabled -} Bool


initEnv : Flags a g -> IO a g (Either Exit.Repl (Env a g))
initEnv (Flags interpreter mode htmlEnabled) =
  IO.bind getRoot <| \root ->
  IO.bind (MMaybe.traverse IO.pure IO.fmap (Task.run << openModule root) (openedModule mode)) <| \openResult ->
  case MMaybe.sequenceA Either.pure Either.fmap openResult of
    Left err ->
      IO.return (Left err)

    Right source ->
      IO.return <| Right <| Env root interpreter False mode
        (Maybe.withDefault defaultHeader source) htmlEnabled



-- LOOP


type Outcome
  = Loop State
  | End


loop : Env a g -> State -> IO a g ()
loop (Env _ _ _ mode _ _ as env) state =
  IO.bind (read mode) <| \input ->
  IO.bind (eval env state input) <| \outcome ->
  case outcome of
    Loop state_ ->
      loop env state_

    End ->
      Command.clearPrompt



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


read : Mode -> IO a g Input
read mode =
  IO.bind Command.clearInput <| \_ ->
  IO.bind (Command.getLineWithInitial ">\u{2000}" "") <| \maybeLine ->
  case maybeLine of
    {- Nothing ->
      return Exit

    Just -}
    chars ->
      let
        lines = Lines (stripLegacyBackslash chars) []
      in
      case categorize mode lines of
        Done input -> IO.return input
        Continue p -> readMore mode lines p


readMore : Mode -> Lines -> Prefill -> IO a g Input
readMore mode previousLines prefill =
  IO.bind (Command.getLineWithInitial "|\u{2000}" (renderPrefill prefill)) <| \input ->
  case input of
    {- Nothing ->
      return Skip

    Just -}
    chars ->
      let
        lines = addLine (stripLegacyBackslash chars) previousLines
      in
      case categorize mode lines of
        Done input_ -> IO.return input_
        Continue p -> readMore mode lines p


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


categorize : Mode -> Lines -> CategorizedInput
categorize mode lines =
  if isBlank lines                         then Done Skip
  else if startsWithColon lines            then toCommand mode lines
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
        PD.Value _ (A.At _ (Src.Value (A.At _ name) _ _ _)) -> ifDone lines (Decl name src)
        PD.Union _ (A.At _ (Src.Union (A.At _ name) _ _  )) -> ifDone lines (Type name src)
        PD.Alias _ (A.At _ (Src.Alias (A.At _ name) _ _  )) -> ifDone lines (Type name src)
        PD.Port  _ _                                        -> Done Port

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


{- NEW: toCommand signature -}
toCommand : Mode -> Lines -> CategorizedInput
toCommand mode lines =
  case String.dropLeft 1 <| String.trimLeft (getFirstLine lines) of
    "reset"       -> Done <| Reset
    "exit"        -> Done <| Exit
    "quit"        -> if isBreakpoint mode
                     then toCommand mode (Lines ":resume" [])
                     else Done <| Exit
    {- NEW: force_quit_ -}
    "force_quit_" -> Done <| Exit
    "help"        -> Done <| Help Nothing
    {- NEW: resume -}
    "resume"      -> if isBreakpoint mode
                     then categorize mode (Lines "Breakpoint.resume bp" [])
                     else Done <| Help (Just "resume")
    rest          -> if isBreakpoint mode && String.startsWith "resume " rest && String.trimLeft (String.dropLeft 7 rest) /= ""
                     then categorize mode (Lines ("Breakpoint.resumeWith bp <|" ++ String.dropLeft 6 rest) [])
                     else Done <| Help (List.head <| String.words rest)


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

    (Report.Report _ (A.Region (A.Position row col) _) _ _) = report
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
  P.bind (P.specialize err_ PT.expression) <| \(_, _) ->
  P.bind (PS.checkFreshLine err) <| \_ ->
  P.return name



-- MODE


{- NEW: openedModule -}
openedModule : Mode -> Maybe String
openedModule mode =
  case mode of
    Normal -> Nothing
    Module moduleName -> Just moduleName
    Breakpoint moduleName _ _ -> Just moduleName


{- NEW: generatedModule -}
generatedModule : Mode -> String
generatedModule mode =
  (Maybe.withDefault N.replModule (openedModule mode))



-- STATE


type State =
  State
    {- imports -} (Map.Map N.Name String)
    {- types -} (Map.Map N.Name String)
    {- decls -} (Map.Map N.Name String)

setImports imports (State _ b c) = State imports b c
setTypes types (State a _ c) = State a types c
setDecls decls (State a b _) = State a b decls


{- NEW: initialState env -}
initialState : Env a g -> State
initialState (Env _ _ _ mode _ _) =
  case mode of
    Breakpoint _ id bpName -> initialBreakpointState id bpName
    {- NEW: force_quit_ -}
    _ -> State Map.empty Map.empty <| Map.singleton "force_quit_" "force_quit_ () = False\n"


{- NEW: initialBreakpointState -}
initialBreakpointState : String -> String -> State
initialBreakpointState id bpName =
  State Map.empty Map.empty <| Map.fromList
    [ ("bp", "bp = Breakpoint.initRepl \"" ++ id ++ "\" " ++ bpName ++ "\n")
    , ("bpArg", "bpArg = case Breakpoint.arg bp of\n  Just x -> x\n  _ -> Debug.todo \"no suspended Breakpoint\"\n")
    , ("bpTag", "bpTag = case Breakpoint.tag bp of\n  Just x -> x\n  _ -> Debug.todo \"no suspended Breakpoint\"\n")
    , ("force_quit_", "force_quit_ () = not (Breakpoint.isSuspended bp)\n")
    ]



-- EVAL


eval : Env a g -> State -> Input -> IO a g Outcome
eval ((Env _ _ _ mode _ _) as env) ((State imports types decls) as state) input =
  case input of
    Skip ->
      IO.return (Loop state)

    Exit ->
      IO.return End

    Reset ->
      IO.bindSequence
        [ Command.clearStdOut
        , printWelcomeMessage
        ]
        (IO.return (Loop (initialState env)))

    Help maybeUnknownCommand ->
      IO.bind (Command.putTemporary (toHelpMessage (isBreakpoint mode) maybeUnknownCommand)) <| \_ ->
      IO.return (Loop state)

    Import name src ->
      let newState = setImports (Map.insert name src imports) state in
      IO.fmap Loop <| attemptEval env state newState OutputNothing

    Type name src ->
      let newState = setTypes (Map.insert name src types) state in
      IO.fmap Loop <| attemptEval env state newState OutputNothing

    Port ->
      IO.bind (Command.putLine "I cannot handle port declarations.") <| \_ ->
      IO.return (Loop state)

    Decl name src ->
      let newState = setDecls (Map.insert name src decls) state in
      IO.fmap Loop <| attemptEval env state newState (OutputDecl name)

    Expr src ->
      IO.fmap Loop <| attemptEval env state state (OutputExpr src)



-- ATTEMPT EVAL


type Output
  = OutputNothing
  | OutputDecl N.Name
  | OutputExpr String


attemptEval : Env a g -> State -> State -> Output -> IO a g State
attemptEval (Env root interpreter ansi mode modulePrefix htmlEnabled) oldState newState output =
  IO.bind
    (Task.run <|
      Task.bind
        (Task.eio Exit.ReplBadDetails <|
          Details.load root) <| \details ->

      Task.bind
        (Task.eio identity <|
          Build.fromRepl root details (toByteString modulePrefix newState output)) <| \artifacts ->

      MMaybe.traverse Task.pure Task.fmap (Task.mapError Exit.ReplBadGenerate << Generate.repl root details ansi htmlEnabled artifacts) (toPrintName output)) <| \result ->

  case result of
    Left exit ->
      IO.bind (interpreter (ShowError exit)) <| \_ ->
      IO.return oldState

    Right Nothing ->
      IO.return newState

    Right (Just (kind, javascript)) ->
      IO.bind (interpreter (inputForKind kind (generatedModule mode) javascript)) <| \interpreterResult ->
      case interpreterResult of
        InterpreterSuccess -> IO.return newState
        InterpreterFailure -> IO.return oldState


{- NEW: inputForKind -}
inputForKind : JS.CodeKind -> N.Name -> String -> InterpreterInput
inputForKind kind moduleName =
  case kind of
    JS.ValueKind ->
      InterpretValue

    JS.HtmlKind ->
      InterpretHtml moduleName



-- TO BYTESTRING


toByteString : String -> State -> Output -> String
toByteString modulePrefix (State imports types decls) output =
  String.concat
    [ modulePrefix, "\n"
    , Map.foldr (++) "" imports
    , Map.foldr (++) "" types
    , Map.foldr (++) "" decls
    , outputToBuilder output
    ]


defaultHeader : String
defaultHeader =
  "module " ++ N.toBuilder N.replModule ++ " exposing (..)"


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


toHelpMessage : Bool -> Maybe String -> String
toHelpMessage showResume maybeBadCommand =
  case maybeBadCommand of
    Nothing ->
      (genericHelpMessage showResume)

    Just command ->
      "I do not recognize the :" ++ command ++ " command. " ++ (genericHelpMessage showResume)


genericHelpMessage : Bool -> String
genericHelpMessage showResume =
  "Valid commands include:\n"
  ++ "\n"
  ++ "  :exit    Exit the REPL\n"
  ++ "  :help    Show this information\n"
  ++ "  :reset   Clear all previous imports and definitions\n"
  ++ (if showResume then "  :resume  Resume from current breakpoint\n" else "")
  ++ "\n"
  ++ "More info at " ++ D.makeLink "repl" ++ "\n"



-- GET ROOT


getRoot : IO a g FilePath
getRoot =
  IO.bind Stuff.findRoot <| \maybeRoot ->
  case maybeRoot of
    Just root ->
      IO.return root

    Nothing ->
      IO.bind Stuff.getReplCache <| \cache ->
      let root = SysFile.addName cache "tmp" in
      IO.bind (SysFile.createDirectoryIfMissing True (SysFile.addName root "src")) <| \_ ->
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



-- OPEN MODULE


{- NEW: openModule -}
openModule : FilePath -> String -> Task z a g String
openModule root input =
  let src = "import " ++ input ++ "\n" in
  Task.bind (Task.eio (Exit.ReplBadInput src) <| parseOpenModuleName src) <| \name ->
  Task.bind (Task.eio Exit.ReplBadDetails <| Details.load root) <| \details ->
  Task.bind (Task.eio (Exit.ReplBadInput src) <| findOpenModulePath root details src name) <| \path ->
  Task.io (File.readUtf8 path)


type alias Task z a g v =
  Task.Task z (GlobalState a g) Exit.Repl v


parseOpenModuleName : String -> IO a g (Either E.Error N.Name)
parseOpenModuleName src =
  IO.return <| case P.fromByteString PM.chompImport ES.ModuleBadEnd src of
    Right (Src.Import (A.At _ name) _ _) ->
      Right name

    Left err ->
      Left <| E.BadSyntax <| ES.ParseError err


findOpenModulePath : FilePath -> Details.Details -> String -> N.Name -> IO a g (Either E.Error FilePath)
findOpenModulePath root details src name =
  IO.rmap (Build.findModulePath root details name) <| \maybePath ->
    case maybePath of
      Just path ->
        Right path

      Nothing ->
        Left <| E.BadImports <| NE.singleton <|
          let region = A.Region (A.Position 1 8) (A.Position 1 (String.length src)) in
          EI.Error region name Set.empty EI.NotFound
