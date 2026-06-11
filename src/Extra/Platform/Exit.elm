module Extra.Platform.Exit exposing
    ( ExitCode(..)
    , exitFailure
    , exitSuccess
    , exitWith
    )

import Extra.Platform as Platform


type alias IO b c d e v =
    Platform.IO b c d e v


type ExitCode
    = ExitSuccess
    | ExitFailure Int


exitSuccess : IO b c d e v
exitSuccess =
    exitWith ExitSuccess


exitFailure : IO b c d e v
exitFailure =
    exitWith (ExitFailure 1)


exitWith : ExitCode -> IO b c d e v
exitWith code =
    case code of
        ExitSuccess ->
            Platform.exitSuccess

        ExitFailure n ->
            Platform.exitFailure n
