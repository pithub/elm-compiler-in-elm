{- MANUALLY FORMATTED -}
module Terminal.Impl.Terminal.Chomp exposing
  ( Chomper(..)
  --
  , Args
  , RequiredArgs
  , Flags
  , Flag
  , Chunk
  , Suggest
  , chompNormalFlag
  , chompOnOffFlag
  , ap
  , return
  , chompFuncArg
  , chompExactly
  , chompMultiple, chomp
  )


import Extra.System.IO as IO exposing (IO)
import Extra.Type.Either as Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe
import Terminal.Impl.Terminal.Error as Error
import Terminal.Impl.Terminal.Internal as Internal



-- GADT REPLACING TYPES


type alias Args s a =
  Internal.Args (Suggest s -> TList Chunk -> Int -> (Suggest s, Either (Error.ArgError s) a))


type alias CompleteArgs s a =
  Internal.CompleteArgs (Suggest s -> TList Chunk -> Int -> (Suggest s, Either (Error.ArgError s) a))


type alias RequiredArgs z s a =
  Internal.RequiredArgs (Int -> Chomper z s (Error.ArgError s) a)


type alias Flags z s v =
  Internal.Flags (Chomper z s (Error.FlagError s) v)


type alias Flag z s v =
  Internal.Flag (Chomper z s (Error.FlagError s) v)



-- CHOMP INTERFACE


chomp : Maybe Int -> TList String -> Args s args -> Flags ( IO s (TList String), Either (Error.Error s) ( args, flags ) ) s flags -> ( IO s (TList String), Either (Error.Error s) ( args, flags ) )
chomp maybeIndex strings args flags =
  let
    (Chomper flagChomper) =
      chompFlags flags

    ok suggest chunks flagValue =
      Tuple.mapSecond (Either.fmap (\argValue -> (argValue,flagValue))) <| chompArgs suggest chunks args

    err suggest flagError =
      ( addSuggest (IO.return []) suggest, Left (Error.BadFlag flagError) )
  in
  flagChomper (toSuggest maybeIndex) (toChunks strings) ok err


toChunks : TList String -> TList Chunk
toChunks strings =
  MList.zipWith Chunk (MList.range 1 (MList.length strings)) strings


toSuggest : Maybe Int -> Suggest s
toSuggest maybeIndex =
  case maybeIndex of
    Nothing ->
      NoSuggestion

    Just index ->
      Suggest index



-- CHOMPER


type Chomper result s x v =
  Chomper (
    Suggest s
    -> TList Chunk
    -> (Suggest s -> TList Chunk -> v -> result)
    -> (Suggest s -> x -> result)
    -> result
  )


type Chunk =
  Chunk
    {- index -} Int
    {- chunk -} String


type Suggest s
  = NoSuggestion
  | Suggest Int
  | Suggestions (IO s (TList String))


makeSuggestion : Suggest s -> (Int -> Maybe (IO s (TList String))) -> Suggest s
makeSuggestion suggest maybeUpdate =
  case suggest of
    NoSuggestion ->
      suggest

    Suggestions _ ->
      suggest

    Suggest index ->
      MMaybe.maybe suggest Suggestions (maybeUpdate index)



-- ARGS

chompArgs : Suggest s -> TList Chunk -> Args s v -> (IO s (TList String), Either (Error.Error s) v)
chompArgs suggest chunks (Internal.Args completeArgsList) =
  chompArgsHelp suggest chunks completeArgsList [] []


chompArgsHelp : Suggest s -> TList Chunk -> TList (CompleteArgs s v) -> TList (Suggest s) -> TList (CompleteArgs s v, Error.ArgError s) -> (IO s (TList String), Either (Error.Error s) v)
chompArgsHelp suggest chunks completeArgsList revSuggest revArgErrors =
  case completeArgsList of
    [] ->
      ( MList.foldl addSuggest (IO.return []) revSuggest
      , Left (Error.BadArgs (MList.map (Tuple.mapFirst Internal.completeArgsKind) (MList.reverse revArgErrors)))
      )

    completeArgs :: others ->
      case chompCompleteArgs suggest chunks completeArgs of
        (s1, Left argError) ->
          chompArgsHelp suggest chunks others (s1::revSuggest) ((completeArgs,argError)::revArgErrors)

        (s1, Right value) ->
          ( addSuggest (IO.return []) s1
          , Right value
          )


addSuggest : IO s (TList String) -> Suggest s -> IO s (TList String)
addSuggest everything suggest =
  case suggest of
    NoSuggestion ->
      everything

    Suggest _ ->
      everything

    Suggestions newStuff ->
      IO.liftA2 (++) newStuff everything



-- COMPLETE ARGS


chompCompleteArgs : Suggest s -> TList Chunk -> CompleteArgs s v -> (Suggest s, Either (Error.ArgError s) v)
chompCompleteArgs suggest chunks completeArgs =
  let
    numChunks = MList.length chunks
  in
  completeArgs.completeResult suggest chunks numChunks


chompExactly : Suggest s -> TList Chunk -> Chomper (Suggest s, Either (Error.ArgError s) v) s (Error.ArgError s) v -> (Suggest s, Either (Error.ArgError s) v)
chompExactly suggest chunks (Chomper chomper) =
  let
    ok s cs value =
      case MList.map (\(Chunk _ chunk) -> chunk) cs of
        [] -> (s, Right value)
        es -> (s, Left (Error.ArgExtras es))

    err s argError =
      (s, Left argError)
  in
  chomper suggest chunks ok err


chompMultiple : Suggest s -> TList Chunk -> Chomper (Suggest s, Either (Error.ArgError s) w) s (Error.ArgError s) (TList v -> w) -> Internal.Parser s v -> (Suggest s, Either (Error.ArgError s) w)
chompMultiple suggest chunks (Chomper chomper) parser =
  let
    err s1 argError =
      (s1, Left argError)
  in
  chomper suggest chunks (chompMultipleHelp parser []) err


chompMultipleHelp : Internal.Parser s v -> TList v -> Suggest s -> TList Chunk -> (TList v -> w) -> (Suggest s, Either (Error.ArgError s) w)
chompMultipleHelp parser revArgs suggest chunks func =
  case chunks of
    [] ->
      (suggest, Right (func (MList.reverse revArgs)))

    Chunk index string :: otherChunks ->
      case tryToParse suggest parser index string of
        (s1, Left expectation) ->
          (s1, Left (Error.ArgBad string expectation))

        (s1, Right arg) ->
          chompMultipleHelp parser (arg::revArgs) s1 otherChunks func



-- REQUIRED ARGS


chompFuncArg : Chomper z s (Error.ArgError s) (v -> w) -> Internal.Parser s v -> Int -> Chomper z s (Error.ArgError s) w
chompFuncArg funcArgs argParser numChunks =
  bind funcArgs <| \func ->
  bind (chompArg numChunks argParser) <| \arg ->
  return (func arg)


chompArg : Int -> Internal.Parser s v -> Chomper z s (Error.ArgError s) v
chompArg numChunks (Internal.Parser singular _ _ _ toExamples as parser) =
  Chomper <| \suggest chunks ok err ->
    case chunks of
      [] ->
        let
          newSuggest = makeSuggestion suggest (suggestArg parser numChunks)
          theError = Error.ArgMissing (Error.Expectation singular (toExamples ""))
        in
        err newSuggest theError

      Chunk index string :: otherChunks ->
        case tryToParse suggest parser index string of
          (newSuggest, Left expectation) ->
            err newSuggest (Error.ArgBad string expectation)

          (newSuggest, Right arg) ->
            ok newSuggest otherChunks arg


suggestArg : Internal.Parser s v -> Int -> Int -> Maybe (IO s (TList String))
suggestArg (Internal.Parser _ _ _ toSuggestions _) numChunks targetIndex =
  if numChunks <= targetIndex then
    Just (toSuggestions "")
  else
    Nothing



-- PARSER


tryToParse : Suggest s -> Internal.Parser s v -> Int -> String -> (Suggest s, Either (Error.Expectation s) v)
tryToParse suggest (Internal.Parser singular _ parse toSuggestions toExamples) index string =
  let
    newSuggest =
      makeSuggestion suggest <| \targetIndex ->
        if index == targetIndex then Just (toSuggestions string) else Nothing

    outcome =
      case parse string of
        Nothing ->
          Left (Error.Expectation singular (toExamples string))

        Just value ->
          Right value
  in
  (newSuggest, outcome)



-- FLAGS


chompFlags : Flags z s v -> Chomper z s (Error.FlagError s) v
chompFlags flags =
  bind (chompFlagsHelp flags) <| \value ->
  bind (checkForUnknownFlags flags) <| \_ ->
  return value


chompFlagsHelp : Flags z s v -> Chomper z s (Error.FlagError s) v
chompFlagsHelp flags =
  flags.chomper



-- FLAG


chompOnOffFlag : String -> Chomper z s (Error.FlagError s) Bool
chompOnOffFlag flagName =
  Chomper <| \suggest chunks ok err ->
    case findFlag flagName chunks of
      Nothing ->
        ok suggest chunks False

      Just (FoundFlag before value after) ->
        case value of
          DefNope ->
            ok suggest (before ++ after) True

          Possibly chunk ->
            ok suggest (before ++ chunk :: after) True

          Definitely _ string ->
            err suggest (Error.FlagWithValue flagName string)


chompNormalFlag : String -> Internal.Parser s v -> Chomper z s (Error.FlagError s) (Maybe v)
chompNormalFlag flagName (Internal.Parser singular _ _ _ toExamples as parser) =
  Chomper <| \suggest chunks ok err ->
    case findFlag flagName chunks of
      Nothing ->
        ok suggest chunks Nothing

      Just (FoundFlag before value after) ->
        let
          attempt index string =
            case tryToParse suggest parser index string of
              (newSuggest, Left expectation) ->
                err newSuggest (Error.FlagWithBadValue flagName string expectation)

              (newSuggest, Right flagValue) ->
                ok newSuggest (before ++ after) (Just flagValue)
        in
        case value of
          Definitely index string ->
            attempt index string

          Possibly (Chunk index string) ->
            attempt index string

          DefNope ->
            err suggest (Error.FlagWithNoValue flagName (Error.Expectation singular (toExamples "")))



-- FIND FLAG


type FoundFlag =
  FoundFlag
    {- before -} (TList Chunk)
    {- value -} Value
    {- after -} (TList Chunk)


type Value
  = Definitely Int String
  | Possibly Chunk
  | DefNope


findFlag : String -> TList Chunk -> Maybe FoundFlag
findFlag flagName chunks =
  findFlagHelp [] ("--" ++ flagName) ("--" ++ flagName ++ "=") chunks


findFlagHelp : TList Chunk -> String -> String -> TList Chunk -> Maybe FoundFlag
findFlagHelp revPrev loneFlag flagPrefix chunks =
  let
    succeed value after =
      Just (FoundFlag (MList.reverse revPrev) value after)

    deprefix string =
      String.dropLeft (String.length flagPrefix) string
  in
  case chunks of
    [] ->
      Nothing

    (Chunk index string as chunk) :: rest ->
      if String.startsWith flagPrefix string then
        succeed (Definitely index (deprefix string)) rest

      else if string /= loneFlag then
        findFlagHelp (chunk::revPrev) loneFlag flagPrefix rest

      else
        case rest of
          [] ->
            succeed DefNope []

          (Chunk _ potentialArg as argChunk) :: restOfRest ->
            if String.startsWith "-" potentialArg then
              succeed DefNope rest
            else
              succeed (Possibly argChunk) restOfRest



-- CHECK FOR UNKNOWN FLAGS


checkForUnknownFlags : Flags z s v -> Chomper z s (Error.FlagError s) ()
checkForUnknownFlags flags =
  Chomper <| \suggest chunks ok err ->
    case MList.filter startsWithDash chunks of
      [] ->
        ok suggest chunks ()

      (Chunk _ unknownFlag :: _) as unknownFlags ->
        err
          (makeSuggestion suggest (suggestFlag unknownFlags flags))
          (Error.FlagUnknown unknownFlag (Internal.flagsKind flags))


suggestFlag : TList Chunk -> Flags z s v -> Int -> Maybe (IO s (TList String))
suggestFlag unknownFlags flags targetIndex =
  case unknownFlags of
    [] ->
      Nothing

    Chunk index string :: otherUnknownFlags ->
      if index == targetIndex then
        Just (IO.return (MList.filter (String.startsWith string) (getFlagNames (Internal.flagsKind flags) [])))
      else
        suggestFlag otherUnknownFlags flags targetIndex


startsWithDash : Chunk -> Bool
startsWithDash (Chunk _ string) =
  String.startsWith "-" string


getFlagNames : Internal.FlagsKind -> TList String -> TList String
getFlagNames flagsKind names =
  case flagsKind of
    Internal.FDone ->
      "--help" :: names

    Internal.FMore subFlags flag ->
      getFlagNames subFlags (getFlagName flag :: names)


getFlagName : Internal.FlagKind -> String
getFlagName flag =
  case flag of
    Internal.FFlag name _ _ ->
      "--" ++ name

    Internal.FOnOff name _ ->
      "--" ++ name



-- CHOMPER INSTANCES


pure : v -> Chomper z s x v
pure value =
  Chomper <| \ss cs ok _ ->
    ok ss cs value


ap : Chomper z s x (v -> w) -> Chomper z s x v -> Chomper z s x w
ap (Chomper funcChomper) (Chomper argChomper) =
  Chomper <| \s cs ok err ->
    let
      ok1 s1 cs1 func =
        let
          ok2 s2 cs2 value =
            ok s2 cs2 (func value)
        in
        argChomper s1 cs1 ok2 err
    in
    funcChomper s cs ok1 err


return : v -> Chomper z s x v
return =
  pure


bind : Chomper z s x v -> (v -> Chomper z s x w) -> Chomper z s x w
bind (Chomper aChomper) callback =
  Chomper <| \s cs ok err ->
    let
      ok1 s1 cs1 a =
        case callback a of
          Chomper bChomper -> bChomper s1 cs1 ok err
    in
    aChomper s cs ok1 err
