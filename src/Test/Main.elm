module Test.Main exposing (main)

{-
   This file is used to
   - Send every module through the compiler
   - Make jfmengels/elm-review-unused happy
-}

import Builder.Generate
import Builder.Reporting.Exit
import Compiler.Reporting.Error.Canonicalize
import Compiler.Reporting.Error.Syntax
import Extra.System.IO
import Extra.System.Path
import Extra.Type.Lens
import Terminal.Develop
import Terminal.Diff
import Terminal.Impl.Terminal.Internal
import Terminal.Main
import Terminal.Make


main : Program Flags Model Msg
main =
    Platform.worker
        { init = Extra.System.IO.init initialModel initialIO
        , update = Extra.System.IO.update
        , subscriptions = \_ -> Sub.none
        }


type alias Flags =
    ()


type alias Model =
    Builder.Generate.GlobalState ()


initialModel : Flags -> Model
initialModel () =
    Builder.Generate.toGlobalState () ()


type alias Msg =
    Extra.System.IO.IO Model ()


initialIO : Flags -> Msg
initialIO () =
    always Terminal.Main.runMain unusedThings


unusedThings : ()
unusedThings =
    let
        toUnit : a -> ()
        toUnit =
            always ()
    in
    toUnit
        [ -- I want to keep these things to have some general purpose modules
          toUnit Builder.Reporting.Exit.toClient
        , toUnit Extra.System.IO.liftCmd
        , toUnit Extra.System.IO.liftCont
        , toUnit Extra.System.IO.log
        , toUnit Extra.System.IO.modifyLens
        , toUnit Extra.System.IO.noOp
        , toUnit Extra.System.IO.now
        , toUnit Extra.System.IO.sequence
        , toUnit Extra.System.IO.sleep
        , toUnit Extra.System.Path.absolute
        , toUnit Extra.System.Path.makeAbsolute
        , toUnit Extra.Type.Lens.compose

        -- at least some parts of these things aren't implemented yet
        , toUnit Builder.Reporting.Exit.ReactorBadBuild
        , toUnit Builder.Reporting.Exit.ReactorBadDetails
        , toUnit Builder.Reporting.Exit.ReactorBadGenerate
        , toUnit Builder.Reporting.Exit.ReactorNoOutline
        , toUnit Builder.Reporting.Exit.reactorToReport
        , toUnit Compiler.Reporting.Error.Syntax.ShaderProblem
        , toUnit <|
            \developFlags ->
                case developFlags of
                    Terminal.Develop.Flags a ->
                        a
        , toUnit <|
            \diffArgs ->
                case diffArgs of
                    Terminal.Diff.CodeVsExactly a ->
                        toUnit a

                    Terminal.Diff.LocalInquiry a b ->
                        toUnit ( a, b )

                    Terminal.Diff.GlobalInquiry a b c ->
                        toUnit ( a, b, c )

                    _ ->
                        ()
        , toUnit <|
            \makeFlags ->
                case makeFlags of
                    Terminal.Make.Flags a b c d e ->
                        ( ( a, b, c ), d, e )

        -- at least some parts of these things are not used even in the original compiler
        , toUnit Compiler.Reporting.Error.Canonicalize.BadPattern
        , toUnit Compiler.Reporting.Error.Canonicalize.DuplicateBinop
        , toUnit <|
            \syntaxModule ->
                case syntaxModule of
                    Compiler.Reporting.Error.Syntax.Declarations a b c ->
                        toUnit ( a, b, c )

                    _ ->
                        ()
        , toUnit <|
            \syntaxLet ->
                case syntaxLet of
                    Compiler.Reporting.Error.Syntax.LetDefAlignment a b c ->
                        toUnit ( a, b, c )

                    _ ->
                        ()
        , toUnit Terminal.Impl.Terminal.Internal.Optional
        ]
