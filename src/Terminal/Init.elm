{- MANUALLY FORMATTED -}
module Terminal.Init exposing
  ( run
  )


import Builder.Deps.Solver as Solver
import Builder.Elm.Outline as Outline
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Constraint as Con
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D exposing (d)
import Extra.Platform as Platform
import Extra.System.IO as IO
import Extra.System.Path as Path
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList
import Extra.Type.Map as Map



-- IO


type alias IO b c d e v =
  Platform.IO b c d e v



-- RUN


run : () -> () -> IO b c d e ()
run () () =
  Reporting.attempt Exit.initToReport runEither


runEither : IO b c d e (Either Exit.Init ())
runEither =
  IO.bind (Platform.doesFileExist (Path.fromString "elm.json")) <| \exists ->
  if exists
    then IO.return (Left Exit.InitAlreadyExists)
    else
      IO.bind (Reporting.ask question) <| \approved ->
      if approved
        then init
        else
          IO.bind (Platform.consoleWrite "Okay, I did not make any changes!\n") <| \_ ->
          IO.return (Right ())


question : D.Doc
question =
  D.stack
    [ D.fillSep
        [d"Hello!"
        ,d"Elm",d"projects",d"always",d"start",d"with",d"an",D.greenS "elm.json",d"file."
        ,d"I",d"can",d"create",d"them!"
        ]
    , D.reflow <|
        "Now you may be wondering, what will be in this file? How do I add Elm files to"
        ++ " my project? How do I see it in the browser? How will my code grow? Do I need"
        ++ " more directories? What about tests? Etc."
    , D.fillSep
        [d"Check",d"out",D.cyan (D.fromChars (D.makeLink "init"))
        ,d"for",d"all",d"the",d"answers!"
        ]
    , d"Knowing all that, would you like me to create an elm.json file now? [Y/n]: "
    ]



-- INIT


init : IO b c d e (Either Exit.Init ())
init =
  IO.bind Solver.initEnv <| \eitherEnv ->
  case eitherEnv of
    Left problem ->
      IO.return (Left (Exit.InitRegistryProblem problem))

    Right (Solver.Env cache _ connection registry) ->
      IO.bind (Solver.verify cache connection registry defaults) <| \result ->
      case result of
        Solver.Err exit ->
          IO.return (Left (Exit.InitSolverProblem exit))

        Solver.NoSolution ->
          IO.return (Left (Exit.InitNoSolution (Map.keys defaults |> MList.map Pkg.fromComparable)))

        Solver.NoOfflineSolution ->
          IO.return (Left (Exit.InitNoOfflineSolution (Map.keys defaults |> MList.map Pkg.fromComparable)))

        Solver.Ok details ->
          let
            solution = Map.map (\(Solver.Details vsn _) -> vsn) details
            directs = Map.intersection solution defaults
            indirects = Map.difference solution defaults
          in
          IO.bind (Platform.createDirectoryIfMissing (Path.fromString "src")) <| \_ ->
          IO.bind (Outline.write (Path.fromString "") <| Outline.App <|
            Outline.AppOutline V.compiler (NE.CList (Outline.RelativeSrcDir (Path.fromString "src")) []) directs indirects Map.empty Map.empty) <| \_ ->
          IO.bind (Platform.consoleWrite "Okay, I created it. Now read that link!\n") <| \_ ->
          IO.return (Right ())


defaults : Map.Map Pkg.Comparable Con.Constraint
defaults =
  Map.fromList
    [ (Pkg.toComparable Pkg.core, Con.anything)
    , (Pkg.toComparable Pkg.browser, Con.anything)
    , (Pkg.toComparable Pkg.html, Con.anything)
    ]
