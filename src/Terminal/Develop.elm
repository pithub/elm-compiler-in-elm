{- MANUALLY FORMATTED -}
module Terminal.Develop exposing
  ( Flags(..)
  , run
  --
  , compile
  )


import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Generate as Generate
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.Data.NonEmptyList as NE
import Extra.Platform as Platform
import Extra.System.IO as IO
import Extra.System.Path exposing (FilePath)
import Extra.Type.Either exposing (Either(..))



-- IO


type alias IO e v =
  Generate.IO e v



-- RUN THE DEV SERVER


type Flags =
  Flags
    {- port -} (Maybe Int)


run : () -> Flags -> IO e ()
run () _ =
  Platform.consoleError "elm reactor: not yet implemented"



-- SERVE ELM


compile : FilePath -> IO e (Either Exit.Reactor String)
compile path =
  IO.bind Stuff.findRoot <| \maybeRoot ->
  case maybeRoot of
    Nothing ->
      IO.return <| Left <| Exit.ReactorNoOutline

    Just root ->
      Task.run <|
        Task.bind (Task.eio Exit.ReactorBadDetails <| Details.load Reporting.silent root) <| \details ->
        Task.bind (Task.eio Exit.ReactorBadBuild <| Build.fromPaths Reporting.silent root details (NE.CList path [])) <| \artifacts ->
        Task.mapError Exit.ReactorBadGenerate <| Generate.dev root details artifacts
