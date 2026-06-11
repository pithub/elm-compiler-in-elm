{- MANUALLY FORMATTED -}
module Terminal.Impl.Terminal.Error exposing
  ( Error(..)
  , ArgError(..)
  , FlagError(..)
  , Expectation(..)
  , exitWithHelp
  , exitWithError
  , exitWithUnknown
  , exitWithOverview
  )


import Compiler.Reporting.Doc as D exposing (d,da)
import Compiler.Reporting.Suggest as Suggest
import Extra.Platform as Platform
import Extra.Platform.Exit as Exit
import Extra.System.IO as IO
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe
import Terminal.Impl.Terminal.Internal as Internal
import Extra.Platform.Handle as Handle



-- STATE AND IO


type alias GlobalState b c d e =
  Platform.GlobalState b c d e


type alias IO b c d e v =
  Platform.IO b c d e v



-- ERROR


type Error s
  = BadArgs (TList (Internal.CompleteArgsKind, ArgError s))
  | BadFlag (FlagError s)


type ArgError s
  = ArgMissing (Expectation s)
  | ArgBad String (Expectation s)
  | ArgExtras (TList String)


type FlagError s
  = FlagWithValue String String
  | FlagWithBadValue String String (Expectation s)
  | FlagWithNoValue String (Expectation s)
  | FlagUnknown String Internal.FlagsKind


type Expectation s =
  Expectation
    {- type -} String
    {- examples -} (IO.IO s (TList String))


-- EXIT


exitSuccess : TList D.Doc -> IO b c d e v
exitSuccess docs =
  exitWith Exit.ExitSuccess docs


exitFailure : TList D.Doc -> IO b c d e v
exitFailure docs =
  exitWith (Exit.ExitFailure 1) docs


exitWith : Exit.ExitCode -> TList D.Doc -> IO b c d e v
exitWith code docs =
  IO.bind (Handle.hIsTerminalDevice Handle.stderr) <| \isTerminal ->
  let adjust = if isTerminal then identity else D.plain in
  IO.bind (D.toAnsi Handle.stderr <|
    adjust <| D.vcat <| MList.concatMap (\doc -> [doc,d""]) docs) <| \_ ->
  IO.bind (Handle.hPutStr Handle.stderr "\n") <| \_ ->
  Exit.exitWith code


getExeName : IO b c d e String
getExeName =
  IO.return "elm" -- hard coded as in other files



-- HELP


exitWithHelp : Maybe String -> String -> D.Doc -> TList Internal.CompleteArgsKind -> Internal.FlagsKind -> IO b c d e ()
exitWithHelp maybeCommand details example args flags =
  IO.bind (toCommand maybeCommand) <| \command ->
  exitSuccess <|
    [ D.reflow details
    , D.indent 4 <| D.cyan <| D.vcat <| MList.map (argsToDoc command) args
    , example
    ]
    ++
      case flagsToDocs flags [] of
        [] ->
          []

        (_::_) as docs ->
          [ d"You can customize this command with the following flags:"
          , D.indent 4 <| D.stack docs
          ]


toCommand : Maybe String -> IO b c d e String
toCommand maybeCommand =
  IO.bind getExeName <| \exeName ->
  IO.return <|
    case maybeCommand of
      Nothing ->
        exeName

      Just command ->
        exeName ++ " " ++ command


argsToDoc : String -> Internal.CompleteArgsKind -> D.Doc
argsToDoc command args =
  case args of
    Internal.Exactly required ->
      argsToDocHelp command required []

    Internal.Multiple required plural ->
      argsToDocHelp command required ["zero or more " ++ plural]

    Internal.Optional required singular ->
      argsToDocHelp command required ["optional " ++ singular]


argsToDocHelp : String -> Internal.RequiredArgsKind -> TList String -> D.Doc
argsToDocHelp command args names =
  case args of
    Internal.Done ->
      D.hang 4 <| D.hsep <| MList.map D.fromChars <|
        command :: MList.map toToken names

    Internal.Required others singular ->
      argsToDocHelp command others (singular :: names)


toToken : String -> String
toToken string =
  "<" ++ String.map (\c -> if c == ' ' then '-' else c) string ++ ">"


flagsToDocs : Internal.FlagsKind -> TList D.Doc -> TList D.Doc
flagsToDocs flags docs =
  case flags of
    Internal.FDone ->
      docs

    Internal.FMore more flag ->
      let
        flagDoc =
          D.vcat <|
            case flag of
              Internal.FFlag name singular description ->
                [ D.dullcyan <| D.fromChars <| "--" ++ name ++ "=" ++ toToken singular
                , D.indent 4 <| D.reflow description
                ]

              Internal.FOnOff name description ->
                [ D.dullcyan <| D.fromChars <| "--" ++ name
                , D.indent 4 <| D.reflow description
                ]
      in
      flagsToDocs more (flagDoc::docs)



-- OVERVIEW


exitWithOverview : D.Doc -> D.Doc -> TList (Internal.Command (GlobalState b c d e)) -> IO b c d e ()
exitWithOverview intro outro commands =
  IO.bind getExeName <| \exeName ->
    exitSuccess
      [ intro
      , d"The most common commands are:"
      , D.indent 4 <| D.stack <| MMaybe.mapMaybe (toSummary exeName) commands
      , d"There are a bunch of other commands as well though. Here is a full list:"
      , D.indent 4 <| D.dullcyan <| toCommandList exeName commands
      , d"Adding the --help flag gives a bunch of additional details about each one."
      , outro
      ]


toSummary : String -> Internal.Command (GlobalState b c d e) -> Maybe D.Doc
toSummary exeName (Internal.Command name summary args _) =
  case summary of
    Internal.Uncommon ->
      Nothing

    Internal.Common summaryString ->
      Just <|
        D.vcat
          [ D.cyan <| argsToDoc (exeName ++ " " ++ name) (MList.head args)
          , D.indent 4 <| D.reflow summaryString
          ]


toCommandList : String -> TList (Internal.Command (GlobalState b c d e)) -> D.Doc
toCommandList exeName commands =
  let
    names = MList.map Internal.toName commands
    width = MList.maximum (MList.map String.length names)

    toExample name =
      D.fromChars <| exeName ++ " " ++ name ++ String.repeat (width - String.length name) " " ++ " --help"
  in
  D.vcat (MList.map toExample names)



-- UNKNOWN


exitWithUnknown : String -> TList String -> IO b c d e ()
exitWithUnknown unknown knowns =
  let
    nearbyKnowns =
      MList.takeWhile (\(r,_) -> r <= 3) (Suggest.rank unknown identity knowns)

    suggestions =
      case MList.map D.greenS (MList.map Tuple.second nearbyKnowns) of
        [] ->
          []

        [nearby] ->
          [d"Try",nearby,d"instead?"]

        [a,b] ->
          [d"Try",a,d"or",b,d"instead?"]

        (_::_::_::_) as abcs ->
          d"Try" :: MList.map (\x -> da[x, d","]) (MList.init abcs) ++ [d"or",MList.last abcs,d"instead?"]
  in
  IO.bind getExeName <| \exeName ->
  exitFailure
    [ D.fillSep <| [d"There",d"is",d"no",D.redS unknown,d"command."] ++ suggestions
    , D.reflow <| "Run `" ++ exeName ++ "` with no arguments to get more hints."
    ]



-- ERROR TO DOC


exitWithError : Error (GlobalState b c d e) -> IO b c d e ()
exitWithError err =
  IO.andThen exitFailure <|
  case err of
    BadFlag flagError ->
      flagErrorToDocs flagError

    BadArgs argErrors ->
      case argErrors of
        [] ->
          IO.return
            [ D.reflow <| "I was not expecting any arguments for this command."
            , D.reflow <| "Try removing them?"
            ]

        [(_, argError)] ->
          argErrorToDocs argError

        _::_::_ ->
          argErrorToDocs <| MList.head <| MList.sortOn toArgErrorRank (MList.map Tuple.second argErrors)


toArgErrorRank : ArgError (GlobalState b c d e) -> Int -- lower is better
toArgErrorRank err =
  case err of
    ArgBad _ _   -> 0
    ArgMissing _ -> 1
    ArgExtras _  -> 2



-- ARG ERROR TO DOC


argErrorToDocs : ArgError (GlobalState b c d e) -> IO b c d e (TList D.Doc)
argErrorToDocs argError =
  case argError of
    ArgMissing (Expectation tipe makeExamples) ->
      IO.bind makeExamples <| \examples ->
      IO.return
        [ D.fillSep
            [d"The",d"arguments",d"you",d"have",d"are",d"fine,",d"but",d"in",d"addition,",d"I",d"was"
            ,d"expecting",d"a",D.yellowS (toToken tipe),d"value.",d"For",d"example:"
            ]
        , D.indent 4 <| D.green <| D.vcat <| MList.map D.fromChars examples
        ]

    ArgBad string (Expectation tipe makeExamples) ->
      IO.bind makeExamples <| \examples ->
      IO.return
        [ d"I am having trouble with this argument:"
        , D.indent 4 <| D.red <| D.fromChars string
        , D.fillSep <|
            [d"It",d"is",d"supposed",d"to",d"be",d"a"
            ,D.yellowS (toToken tipe),d"value,",d"like"
            ] ++ if MList.length examples == 1 then [d"this:"] else [d"one",d"of",d"these:"]
        , D.indent 4 <| D.green <| D.vcat <| MList.map D.fromChars examples
        ]

    ArgExtras extras ->
      let
        (these, them) =
          case extras of
            [_] -> ("this argument", "it")
            _ -> ("these arguments", "them")
      in
      IO.return
        [ D.reflow <| "I was not expecting " ++ these ++ ":"
        , D.indent 4 <| D.red <| D.vcat <| MList.map D.fromChars extras
        , D.reflow <| "Try removing " ++ them ++ "?"
        ]



-- FLAG ERROR TO DOC


flagErrorHelp : String -> String -> TList D.Doc -> IO b c d e (TList D.Doc)
flagErrorHelp summary original explanation =
  IO.return <|
    [ D.reflow summary
    , D.indent 4 (D.redS original)
    ]
    ++ explanation


flagErrorToDocs : FlagError (GlobalState b c d e) -> IO b c d e (TList D.Doc)
flagErrorToDocs flagError =
  case flagError of
    FlagWithValue flagName value ->
      flagErrorHelp
        "This on/off flag was given a value:"
        ("--" ++ flagName ++ "=" ++ value)
        [ d("An on/off flag either exists or not. It cannot have an equals sign and value.\n"
          ++ "Maybe you want this instead?")
        , D.indent 4 <| D.greenS <| "--" ++ flagName
        ]

    FlagWithNoValue flagName (Expectation tipe makeExamples) ->
      IO.bind makeExamples <| \examples ->
      flagErrorHelp
        "This flag needs more information:"
        ("--" ++ flagName)
        [ D.fillSep [d"It",d"needs",d"a",D.yellowS (toToken tipe),d"like",d"this:"]
        , D.indent 4 <| D.vcat <| MList.map D.greenS <|
            case MList.take 4 examples of
              [] ->
                ["--" ++ flagName ++ "=" ++ toToken tipe]

              _::_ ->
                MList.map (\example -> "--" ++ flagName ++ "=" ++ example) examples
        ]

    FlagWithBadValue flagName badValue (Expectation tipe makeExamples) ->
      IO.bind makeExamples <| \examples ->
      flagErrorHelp
        "This flag was given a bad value:"
        ("--" ++ flagName ++ "=" ++ badValue)
        [ D.fillSep <|
            [d"I",d"need",d"a",D.yellowS (toToken tipe),d"value.",d"For",d"example:"
            ]
        , D.indent 4 <| D.vcat <| MList.map D.greenS <|
            case MList.take 4 examples of
              [] ->
                ["--" ++ flagName ++ "=" ++ toToken tipe]

              _::_ ->
                MList.map (\example -> "--" ++ flagName ++ "=" ++ example) examples
        ]

    FlagUnknown unknown flags ->
      flagErrorHelp
        "I do not recognize this flag:"
        unknown
        (
          let unknownName = String.fromList <| MList.takeWhile ((/=) '=') (MList.dropWhile ((==) '-') (String.toList unknown)) in
          case getNearbyFlags unknownName flags [] of
            [] ->
              []

            [thisOne] ->
              [ D.fillSep [d"Maybe",d"you",d"want",D.green thisOne,d"instead?"]
              ]

            suggestions ->
              [ D.fillSep [d"Maybe",d"you",d"want",d"one",d"of",d"these",d"instead?"]
              , D.indent 4 <| D.green <| D.vcat suggestions
              ]
        )


getNearbyFlags : String -> Internal.FlagsKind -> TList (Int, String) -> TList D.Doc
getNearbyFlags unknown flags unsortedFlags =
  case flags of
    Internal.FMore more flag ->
      getNearbyFlags unknown more (getNearbyFlagsHelp unknown flag :: unsortedFlags)

    Internal.FDone ->
      MList.map D.fromChars <| MList.map Tuple.second <| MList.sortOn Tuple.first <|
        case MList.filter (\(d,_) -> d < 3) unsortedFlags of
          [] ->
            unsortedFlags

          nearbyUnsortedFlags ->
            nearbyUnsortedFlags


getNearbyFlagsHelp : String -> Internal.FlagKind -> (Int, String)
getNearbyFlagsHelp unknown flag =
  case flag of
    Internal.FOnOff flagName _ ->
      ( Suggest.distance unknown flagName
      , "--" ++ flagName
      )

    Internal.FFlag flagName singular _ ->
      ( Suggest.distance unknown flagName
      , "--" ++ flagName ++ "=" ++ toToken singular
      )
