{- MANUALLY FORMATTED -}
module Terminal.Impl.Terminal exposing
  ( app
  , Command, command
  , common, uncommon
  , noFlags, flags, andFlag
  , Parser, parser
  , flag, onOff
  , noArgs, zeroOrMore, oneOf
  , require0, require1, require2, require3
  )


import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import Extra.Platform as Platform
import Extra.Platform.Exit as Exit
import Extra.Platform.Handle as Handle
import Extra.System.IO as IO exposing (IO)
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Terminal.Impl.Terminal.Chomp as Chomp
import Terminal.Impl.Terminal.Error as Error
import Terminal.Impl.Terminal.Internal as Internal



-- STATE AND IO


type alias GlobalState b c d e =
  Platform.GlobalState b c d e


type alias IO b c d e v =
  Platform.IO b c d e v



-- FROM INTERNAL


type alias Command s =
  Internal.Command s


command :
  String -> Internal.Summary -> String -> D.Doc
  -> Chomp.Args (GlobalState b c d e) args
  -> (Chomp.Flags
       ( IO b c d e (TList String), Either (Error.Error (GlobalState b c d e)) ( args, flags ) )
       (GlobalState b c d e)
       flags
      )
  -> (args -> flags -> IO b c d e ())
  -> Internal.Command (GlobalState b c d e)
command name summary details example args_ flags_ callback =
  Internal.Command
    name
    summary
    (Internal.argsKind args_)
    (commandHelp details example args_ flags_ callback)


type alias Parser s a =
  Internal.Parser s a


parser :
  String -> String -> (String -> Maybe a)
  -> (String -> IO.IO s (TList String))
  -> (String -> IO.IO s (TList String))
  -> Internal.Parser s a
parser = Internal.Parser


common : String -> Internal.Summary
common = Internal.Common


uncommon : Internal.Summary
uncommon = Internal.Uncommon



-- APP


app : D.Doc -> D.Doc -> TList (Internal.Command (GlobalState b c d e)) -> IO b c d e ()
app intro outro commands =
  IO.bind Platform.getArgs <| \argStrings ->
  case argStrings of
    [] ->
      Error.exitWithOverview intro outro commands

    ["--help"] ->
      Error.exitWithOverview intro outro commands

    ["--version"] ->
      IO.bind (Handle.hPutStr Handle.stdout (V.toChars V.compiler)) <| \_ ->
      Exit.exitSuccess

    command_ :: chunks ->
      case MList.find (\cmd -> Internal.toName cmd == command_) commands of
        Nothing ->
          Error.exitWithUnknown command_ (MList.map Internal.toName commands)

        Just (Internal.Command _ _ _ helper) ->
          helper command_ chunks


commandHelp :
  String -> D.Doc -> Chomp.Args (GlobalState b c d e) args
  -> Chomp.Flags
       ( IO b c d e (TList String), Either (Error.Error (GlobalState b c d e)) ( args, flags ) )
       (GlobalState b c d e)
       flags
  -> (args -> flags -> IO b c d e ())
  -> String -> TList String -> IO b c d e ()
commandHelp details example args_ flags_ callback command_ chunks =
  if MList.elem "--help" chunks then
    Error.exitWithHelp (Just command_) details example (Internal.argsKind args_) (Internal.flagsKind flags_)

  else
    case Tuple.second <| Chomp.chomp Nothing chunks args_ flags_ of
      Right (argsValue, flagsValue) ->
        callback argsValue flagsValue

      Left err ->
        Error.exitWithError err



-- FLAGS


{-|-}
noFlags : Chomp.Flags z (GlobalState b c d e) ()
noFlags =
  flags ()


{-|-}
flags : v -> Chomp.Flags z (GlobalState b c d e) v
flags v =
  { kind = Internal.FDone
  , chomper = Chomp.return v
  }


andFlag :
  Chomp.Flag z (GlobalState b c d e) v
  -> Chomp.Flags z (GlobalState b c d e) (v -> w)
  -> Chomp.Flags z (GlobalState b c d e) w
andFlag arg func =
  { kind = Internal.FMore func.kind arg.kind
  , chomper = Chomp.ap func.chomper arg.chomper
  }



-- FLAG


{-|-}
flag :
  String -> Internal.Parser (GlobalState b c d e) v -> String
  -> Chomp.Flag z (GlobalState b c d e) (Maybe v)
flag flagName (Internal.Parser singular _ _ _ _ as parser_) description =
  { kind = Internal.FFlag flagName singular description
  , chomper = Chomp.chompNormalFlag flagName parser_
  }


{-|-}
onOff :
  String -> String
  -> Chomp.Flag z (GlobalState b c d e) Bool
onOff flagName description =
  { kind = Internal.FOnOff flagName description
  , chomper = Chomp.chompOnOffFlag flagName
  }



-- FANCY ARGS


{-|-}
args : v -> Chomp.RequiredArgs z (GlobalState b c d e) v
args v =
  { kind = Internal.Done
  , chomper = \_ -> Chomp.return v
  }


{-|-}
exactly :
  Chomp.RequiredArgs
    (Chomp.Suggest (GlobalState b c d e), Either (Error.ArgError (GlobalState b c d e)) v)
    (GlobalState b c d e) v
  -> Chomp.Args (GlobalState b c d e) v
exactly requiredArgs =
  Internal.Args
    [ { kind = Internal.Exactly requiredArgs.kind
      , completeResult = \suggest chunks numChunks -> Chomp.chompExactly suggest chunks (requiredArgs.chomper numChunks)
      }
    ]


{-|-}
bang :
  Internal.Parser (GlobalState b c d e) v
  -> Chomp.RequiredArgs z (GlobalState b c d e) (v -> w)
  -> Chomp.RequiredArgs z (GlobalState b c d e) w
bang (Internal.Parser singular _ _ _ _ as argParser) funcArgs =
  { kind = Internal.Required funcArgs.kind singular
  , chomper = \numChunks -> Chomp.chompFuncArg (funcArgs.chomper numChunks) argParser numChunks
  }


{-|-}
dots :
  Internal.Parser (GlobalState b c d e) v
  -> Chomp.RequiredArgs
       (Chomp.Suggest (GlobalState b c d e), Either (Error.ArgError (GlobalState b c d e)) w)
       (GlobalState b c d e)
       (TList v -> w)
  -> Chomp.Args (GlobalState b c d e) w
dots (Internal.Parser _ plural _ _ _ as repeatedArg) requiredArgs =
  Internal.Args
    [ { kind = Internal.Multiple requiredArgs.kind plural
      , completeResult = \suggest chunks numChunks -> Chomp.chompMultiple suggest chunks (requiredArgs.chomper numChunks) repeatedArg
      }
    ]


{-|-}
oneOf : TList (Internal.Args v) -> Internal.Args v
oneOf listOfArgs =
  Internal.Args (MList.concatMap (\(Internal.Args v) -> v) listOfArgs)



-- SIMPLE ARGS


{-|-}
noArgs : Chomp.Args (GlobalState b c d e) ()
noArgs =
  exactly (args ())


{-|-}
zeroOrMore :
  Internal.Parser (GlobalState b c d e) v
  -> Chomp.Args (GlobalState b c d e) (TList v)
zeroOrMore parser_ =
  args identity |> dots parser_


{-|-}
require0 : args -> Chomp.Args (GlobalState b c d e) args
require0 value =
  exactly (args value)


{-|-}
require1 :
  (v1 -> args)
  -> Internal.Parser (GlobalState b c d e) v1
  -> Chomp.Args (GlobalState b c d e) args
require1 func v1 =
  exactly (args func |> bang v1)


{-|-}
require2 :
  (v1 -> v2 -> args)
  -> Internal.Parser (GlobalState b c d e) v1
  -> Internal.Parser (GlobalState b c d e) v2
  -> Chomp.Args (GlobalState b c d e) args
require2 func v1 v2 =
  exactly (args func |> bang v1 |> bang v2)


{-|-}
require3 :
  (v1 -> v2 -> v3 -> args)
  -> Internal.Parser (GlobalState b c d e) v1
  -> Internal.Parser (GlobalState b c d e) v2
  -> Internal.Parser (GlobalState b c d e) v3
  -> Chomp.Args (GlobalState b c d e) args
require3 func v1 v2 v3 =
  exactly (args func |> bang v1 |> bang v2 |> bang v3)
