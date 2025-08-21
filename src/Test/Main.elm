module Test.Main exposing (main)

{-
   This file is used to
   - Send every module through the compiler in `make test`
   - Make `elm-review --template jfmengels/elm-review-unused/example` happy
-}

import Builder.Build
import Builder.Elm.Details
import Builder.Generate
import Builder.Http
import Builder.Reporting.Exit
import Builder.Reporting.Exit.Help
import Extra.System.File
import Extra.System.IO
import Global
import Terminal.Command
import Terminal.Helpers
import Terminal.Init
import Terminal.Install
import Terminal.Main
import Terminal.Make
import Terminal.Reactor
import Terminal.Repl


main : Program () (Terminal.Repl.GlobalState ()) (Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ())
main =
    let
        initialModel : () -> Terminal.Repl.GlobalState ()
        initialModel _ =
            Global.State
                Extra.System.File.initialState
                Builder.Http.initialState
                Builder.Elm.Details.initialState
                Builder.Build.initialState
                Builder.Generate.initialState
                Terminal.Command.initialState
                Terminal.Repl.initialLocalState
                ()

        initialIO : () -> Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ()
        initialIO _ =
            Extra.System.IO.sequence
                [ compilerUiInterface
                , replWorkerInterface
                ]

        toUnit : a -> ()
        toUnit _ =
            ()

        toIO : a -> Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ()
        toIO _ =
            Extra.System.IO.noOp

        compilerUiInterface : Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ()
        compilerUiInterface =
            Extra.System.IO.sequence
                [ Builder.Http.setPrefix |> toIO
                , Builder.Reporting.Exit.MakeNoOutline |> toIO
                , Builder.Reporting.Exit.initToReport |> toIO
                , Builder.Reporting.Exit.installToReport |> toIO
                , Builder.Reporting.Exit.makeToReport |> toIO
                , Builder.Reporting.Exit.reactorToReport |> toIO
                , Builder.Reporting.Exit.replToReport |> toIO
                , Builder.Reporting.Exit.toBuildProblemReport |> toIO
                , Builder.Reporting.Exit.toClient |> toIO
                , Builder.Reporting.Exit.toDetailsReport |> toIO
                , Builder.Reporting.Exit.toRegistryProblemReport |> toIO
                , Extra.System.File.getCurrentDirectoryEntriesPure |> toIO
                , Extra.System.File.getCurrentDirectoryNamesPure |> toIO
                , Extra.System.File.mountRemote |> toIO
                , Extra.System.File.mountStatic |> toIO
                , Extra.System.File.removeDirectory |> toIO
                , Extra.System.File.resetFileSystem |> toIO
                , Extra.System.File.setCurrentDirectory |> toIO
                , Extra.System.File.setMountPrefix |> toIO
                , Extra.System.IO.join |> toIO
                , Extra.System.IO.sleep |> toIO
                , Extra.System.IO.when |> toIO
                , Terminal.Command.clearPutLine |> toIO
                , Terminal.Command.clearStdOut |> toIO
                , Terminal.Command.getDurationSinceLastInput |> toIO
                , Terminal.Command.getLine |> toIO
                , Terminal.Command.getText |> toIO
                , Terminal.Command.gotInput |> toIO
                , Terminal.Command.lensInput |> toIO
                , Terminal.Command.lensPrompt |> toIO
                , Terminal.Command.lensStdOut |> toIO
                , Terminal.Command.setCurrentInput |> toIO
                , Terminal.Command.setInput |> toIO
                , Terminal.Command.setNextInput |> toIO
                , Terminal.Helpers.parsePackage |> toIO
                , Terminal.Init.run |> toIO
                , Terminal.Install.install |> toIO
                , Terminal.Main.runMain |> toIO
                , Terminal.Make.run |> toIO
                , Terminal.Reactor.compile |> toIO
                , Terminal.Repl.Breakpoint |> toIO
                , Terminal.Repl.Flags |> toIO
                , Terminal.Repl.InterpreterFailure |> toIO
                , Terminal.Repl.InterpreterSuccess |> toIO
                , Terminal.Repl.Module |> toIO
                , Terminal.Repl.Normal |> toIO
                , (\interpreterInput ->
                    case interpreterInput of
                        Terminal.Repl.InterpretHtml a b ->
                            ( a, b ) |> toUnit

                        Terminal.Repl.InterpretValue a ->
                            a |> toUnit

                        Terminal.Repl.ShowError a ->
                            a |> toUnit
                  )
                    |> toIO
                , Terminal.Repl.continueInterpreter |> toIO
                , Terminal.Repl.run |> toIO
                ]

        replWorkerInterface : Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ()
        replWorkerInterface =
            Extra.System.IO.sequence
                [ Builder.Http.setPrefix |> toIO
                , Builder.Reporting.Exit.replToReport |> toIO
                , Builder.Reporting.Exit.Help.reportToDoc |> toIO
                , Extra.System.File.mountRemote |> toIO
                , Extra.System.File.mountStatic |> toIO
                , Extra.System.File.setCurrentDirectory |> toIO
                , Extra.System.File.setMountPrefix |> toIO
                , Terminal.Command.clearStdOut |> toIO
                , Terminal.Command.getText |> toIO
                , Terminal.Command.lensStdOut |> toIO
                , Terminal.Repl.Configured |> toIO
                , Terminal.Repl.Flags |> toIO
                , Terminal.Repl.InterpreterFailure |> toIO
                , Terminal.Repl.InterpreterSuccess |> toIO
                , Terminal.Repl.continueInterpreter |> toIO
                , Terminal.Repl.addLine |> toIO
                , Terminal.Repl.categorize |> toIO
                , Terminal.Repl.eval |> toIO
                , Terminal.Repl.initialState |> toIO
                , Terminal.Repl.initEnv |> toIO
                , Terminal.Repl.printWelcomeMessage |> toIO
                , Terminal.Repl.renderPrefill |> toIO
                , Terminal.Repl.stripLegacyBackslash |> toIO
                , (\interpreterInput ->
                    case interpreterInput of
                        Terminal.Repl.InterpretValue a ->
                            a |> toUnit

                        Terminal.Repl.ShowError a ->
                            a |> toUnit

                        x ->
                            x |> toUnit
                  )
                    |> toIO
                ]
    in
    Platform.worker
        { init = Extra.System.IO.init initialModel initialIO
        , subscriptions = \_ -> Sub.none
        , update = Extra.System.IO.update
        }
