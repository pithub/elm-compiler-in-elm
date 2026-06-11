{- MANUALLY FORMATTED -}
module Terminal.Impl.Terminal.Internal exposing
  ( Command(..)
  , toName
  , Summary(..)
  , Flags, FlagsKind(..), flagsKind
  , Flag, FlagKind(..)
  , Parser(..)
  , Args(..), argsKind
  , CompleteArgs, CompleteArgsKind(..), completeArgsKind
  , RequiredArgs, RequiredArgsKind(..)
  )


import Extra.System.IO exposing (IO)
import Extra.Type.List as MList exposing (TList)



-- COMMAND


type Command s =
  Command
    String
    Summary
    ArgsKind
    (String -> TList String -> IO s ())


toName : Command s -> String
toName (Command name _ _ _) =
  name



{-| The information that shows when you run the executable with no arguments.
If you say it is `Common`, you need to tell people what it does. Try to keep
it to two or three lines. If you say it is `Uncommon` you can rely on `Details`
for a more complete explanation.
-}
type Summary = Common String | Uncommon



-- FLAGS


type alias Flags a =
  { kind : FlagsKind
  , chomper : a
  }


flagsKind : Flags a -> FlagsKind
flagsKind =
  .kind


type FlagsKind
  = FDone
  | FMore FlagsKind FlagKind


type alias Flag a =
  { kind : FlagKind
  , chomper : a
  }


type FlagKind
  = FFlag String String String -- Maybe a
  | FOnOff String String -- Bool



-- PARSERS


type Parser s a =
  Parser
    {- singular -} String
    {- plural -} String
    {- parser -} (String -> Maybe a)
    {- suggest -} (String -> IO s (TList String))
    {- examples -} (String -> IO s (TList String))



-- ARGS


type Args a =
  Args (TList (CompleteArgs a))


argsKind : Args a -> ArgsKind
argsKind (Args args) =
  MList.map .kind args


type alias ArgsKind =
  TList CompleteArgsKind


type alias CompleteArgs a =
  { kind : CompleteArgsKind
  , completeResult : a
  }


completeArgsKind : CompleteArgs a -> CompleteArgsKind
completeArgsKind =
  .kind


type CompleteArgsKind
  = Exactly  RequiredArgsKind
  | Multiple RequiredArgsKind String
  | Optional RequiredArgsKind String


type alias RequiredArgs a =
  { kind : RequiredArgsKind
  , chomper : a
  }


type RequiredArgsKind
  = Done
  | Required RequiredArgsKind String
