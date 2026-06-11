{- MANUALLY FORMATTED -}
module Terminal.Make exposing
  ( Flags(..)
  , Output
  , ReportType
  , run
  , reportType
  , output
  , docsFile
  )


import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.File as File
import Builder.Generate as Generate
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Optimized as Opt
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Generate.Html as Html
import Extra.Platform as Platform
import Extra.System.IO as IO
import Extra.System.Path as Path exposing (FilePath)
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe
import Terminal.Impl.Terminal as Terminal



-- STATE AND IO


type alias GlobalState e =
  Generate.GlobalState e


type alias IO e v =
  Generate.IO e v



-- FLAGS


type Flags =
  Flags
    {- debug -} Bool
    {- optimize -} Bool
    {- output -} (Maybe Output)
    {- report -} (Maybe ReportType)
    {- docs -} (Maybe FilePath)


type Output
  = JS FilePath
  | Html FilePath
  | DevNull


type ReportType
  = Json



-- RUN


type alias Task z e v =
  Task.Task z (GlobalState e) Exit.Make v


run : TList FilePath -> Flags -> IO e ()
run paths (Flags _ _ _ report _ as flags) =
  IO.bind (getStyle report) <| \style ->
  Reporting.attemptWithStyle style Exit.makeToReport <|
    runEither paths style flags


runEither : TList FilePath -> Reporting.Style -> Flags -> IO e (Either Exit.Make ())
runEither paths style flags =
  IO.bind Stuff.findRoot <| \maybeRoot ->
  case maybeRoot of
    Just root -> runHelp root paths style flags
    Nothing   -> IO.return <| Left <| Exit.MakeNoOutline


runHelp : FilePath -> TList FilePath -> Reporting.Style -> Flags -> IO e (Either Exit.Make ())
runHelp root paths style (Flags debug optimize maybeOutput _ _) =
  Task.run <|
  Task.bind (getMode debug optimize) <| \desiredMode ->
  Task.bind (Task.eio Exit.MakeBadDetails (Details.load style root)) <| \details ->
  case paths of
    [] ->
      Task.bind (getExposed details) <| \exposed ->
      buildExposed style root details exposed

    p::ps ->
      Task.bind (buildPaths style root details (NE.CList p ps)) <| \artifacts ->
      case maybeOutput of
        Nothing ->
          case getMains artifacts of
            [] ->
              Task.return ()

            [name] ->
              Task.bind (toBuilder root details desiredMode artifacts) <| \builder ->
              generate style (Path.fromString "index.html") (Html.sandwich name builder) (NE.CList name [])

            name::names ->
              Task.bind (toBuilder root details desiredMode artifacts) <| \builder ->
              generate style (Path.fromString "elm.js") builder (NE.CList name names)

        Just DevNull ->
          Task.return ()

        Just (JS target) ->
          case getNoMains artifacts of
            [] ->
              Task.bind (toBuilder root details desiredMode artifacts) <| \builder ->
              generate style target builder (Build.getRootNames artifacts)

            name::names ->
              Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)

        Just (Html target) ->
          Task.bind (hasOneMain artifacts) <| \name ->
          Task.bind (toBuilder root details desiredMode artifacts) <| \builder ->
          generate style target (Html.sandwich name builder) (NE.CList name [])





-- GET INFORMATION


getStyle : Maybe ReportType -> IO e Reporting.Style
getStyle report =
  case report of
    Nothing -> Reporting.terminal
    Just Json -> IO.return Reporting.json


getMode : Bool -> Bool -> Task z e DesiredMode
getMode debug optimize =
  case (debug, optimize) of
    (True , True ) -> Task.throw Exit.MakeCannotOptimizeAndDebug
    (True , False) -> Task.return Debug
    (False, False) -> Task.return Dev
    (False, True ) -> Task.return Prod


getExposed : Details.Details -> Task z e (NE.TList ModuleName.Raw)
getExposed (Details.Details _ validOutline _ _ _ _) =
  case validOutline of
    Details.ValidApp _ ->
      Task.throw Exit.MakeAppNeedsFileNames

    Details.ValidPkg _ exposed _ ->
      case exposed of
        [] -> Task.throw Exit.MakePkgNeedsExposing
        m::ms -> Task.return (NE.CList m ms)



-- BUILD PROJECTS


buildExposed : Reporting.Style -> FilePath -> Details.Details -> NE.TList ModuleName.Raw -> Task z e ()
buildExposed style root details exposed =
  let
    docsGoal = Build.ignoreDocs
  in
  Task.eio Exit.MakeCannotBuild <|
    Build.fromExposed style root details docsGoal exposed


buildPaths : Reporting.Style -> FilePath -> Details.Details -> NE.TList FilePath -> Task z e Build.Artifacts
buildPaths style root details paths =
  Task.eio Exit.MakeCannotBuild <|
    Build.fromPaths style root details paths



-- GET MAINS


getMains : Build.Artifacts -> TList ModuleName.Raw
getMains (Build.Artifacts _ _ roots modules) =
  MMaybe.mapMaybe (getMain modules) (NE.toList roots)


getMain : TList Build.Module -> Build.Root -> Maybe ModuleName.Raw
getMain modules root =
  case root of
    Build.Inside name ->
      if MList.any (isMain name) modules
      then Just name
      else Nothing

    Build.Outside name (Opt.LocalGraph maybeMain _ _) ->
      MMaybe.bind maybeMain <| \_ -> Just name


isMain : ModuleName.Raw -> Build.Module -> Bool
isMain targetName modul =
  case modul of
    Build.Fresh name _ (Opt.LocalGraph maybeMain _ _) ->
      MMaybe.isJust maybeMain && name == targetName

    Build.Cached name mainIsDefined _ ->
      mainIsDefined && name == targetName



-- HAS ONE MAIN


hasOneMain : Build.Artifacts -> Task z e ModuleName.Raw
hasOneMain (Build.Artifacts _ _ roots modules) =
  case roots of
    NE.CList root [] -> Task.mio Exit.MakeNoMain (IO.return <| getMain modules root)
    NE.CList _ (_::_) -> Task.throw Exit.MakeMultipleFilesIntoHtml



-- GET MAINLESS


getNoMains : Build.Artifacts -> TList ModuleName.Raw
getNoMains (Build.Artifacts _ _ roots modules) =
  MMaybe.mapMaybe (getNoMain modules) (NE.toList roots)


getNoMain : TList Build.Module -> Build.Root -> Maybe ModuleName.Raw
getNoMain modules root =
  case root of
    Build.Inside name ->
      if MList.any (isMain name) modules
      then Nothing
      else Just name

    Build.Outside name (Opt.LocalGraph maybeMain _ _) ->
      case maybeMain of
        Just _  -> Nothing
        Nothing -> Just name



-- GENERATE


generate : Reporting.Style -> FilePath -> String -> NE.TList ModuleName.Raw -> Task z e ()
generate style target builder names =
  Task.io <|
    IO.bind (Platform.createDirectoryIfMissing (Path.dropLastName target)) <| \_ ->
    IO.bind (File.writeUtf8 target builder) <| \_ ->
    Reporting.reportGenerate style names target



-- TO BUILDER


type DesiredMode = Debug | Dev | Prod


toBuilder : FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Task z e String
toBuilder root details desiredMode artifacts =
  Task.mapError Exit.MakeBadGenerate <|
    case desiredMode of
      Debug -> Generate.debug root details artifacts
      Dev   -> Generate.dev   root details artifacts
      Prod  -> Generate.prod  root details artifacts



-- PARSERS


reportType : Terminal.Parser (GlobalState e) ReportType
reportType =
  Terminal.parser
    {- singular -} "report type"
    {- plural -} "report types"
    {- parser -} (\string -> if string == "json" then Just Json else Nothing)
    {- suggest -} (\_ -> IO.return ["json"])
    {- examples -} (\_ -> IO.return ["json"])


output : Terminal.Parser (GlobalState e) Output
output =
  Terminal.parser
    {- singular -} "output file"
    {- plural -} "output files"
    {- parser -} parseOutput
    {- suggest -} (\_ -> IO.return [])
    {- examples -} (\_ -> IO.return [ "elm.js", "index.html", "/dev/null" ])


parseOutput : String -> Maybe Output
parseOutput name =
  if      isDevNull name      then Just DevNull
  else if hasExt ".html" name then Just (Html (Path.fromString name))
  else if hasExt ".js"   name then Just (JS (Path.fromString name))
  else                             Nothing


docsFile : Terminal.Parser (GlobalState e) FilePath
docsFile =
  Terminal.parser
    {- singular -} "json file"
    {- plural -} "json files"
    {- parser -} (\name -> if hasExt ".json" name then Just (Path.fromString name) else Nothing)
    {- suggest -} (\_ -> IO.return [])
    {- examples -} (\_ -> IO.return ["docs.json","documentation.json"])


hasExt : String -> String -> Bool
hasExt ext path =
  String.endsWith ext path && String.length path > String.length ext


isDevNull : String -> Bool
isDevNull name =
  name == "/dev/null" || name == "NUL" || name == "$null"
