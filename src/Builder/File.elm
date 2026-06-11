{- MANUALLY FORMATTED -}
module Builder.File exposing
  ( Time, bTime
  , getTime
  , zeroTime
  , writeBinary
  , readBinary
  , writeUtf8
  , readUtf8
  , writeBuilder
  , writePackage
  , exists
  , remove
  , toMillis
  )


import BigInt exposing (BigInt)
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Extra.Data.Binary as B
import Extra.Platform as Platform
import Extra.Platform.Handle as Handle
import Extra.System.IO as IO
import Extra.System.Path as Path exposing (FilePath)
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList
import Time as T
import Zip
import Zip.Entry



-- IO


type alias IO b c d e v =
  Platform.IO b c d e v



-- TIME


type Time = Time T.Posix


getTime : FilePath -> IO b c d e Time
getTime path =
  IO.fmap Time (Platform.getModificationTime path)


toMillis : Time -> Int
toMillis (Time time) =
  T.posixToMillis time


zeroTime : Time
zeroTime =
  Time (T.millisToPosix 0)


bTime : B.Binary Time
bTime =
  B.bin1 bigToTime timeToBig B.bBigInt


bigToTime : BigInt -> Time
bigToTime big =
  Time (T.millisToPosix (B.bigToInt (BigInt.div big bigTimeFactor)))


timeToBig : Time -> BigInt
timeToBig (Time time) =
  BigInt.mul (BigInt.fromInt (T.posixToMillis time)) bigTimeFactor


bigTimeFactor : BigInt
bigTimeFactor =
  BigInt.fromInt (2 * halfTimeFactor)


halfTimeFactor : Int
halfTimeFactor =
  500000000



-- BINARY


writeBinary : B.Binary v -> FilePath -> v -> IO b c d e ()
writeBinary binA path value =
  let dir = Path.dropLastName path in
  IO.bind (Platform.createDirectoryIfMissing dir) <| \_ ->
  Platform.writeFile path (B.encode binA value)


readBinary : B.Binary v -> FilePath -> IO b c d e (Maybe v)
readBinary binA path =
  IO.bind (Platform.readFile path) <| \maybeBytes ->
  case maybeBytes of
    Just bytes ->
      case B.decode binA bytes of
        Right a ->
          IO.return (Just a)

        Left (offset, message) ->
          IO.bind (Handle.hPutStr Handle.stderr <| String.join "\n" <|
            [ "+-------------------------------------------------------------------------------"
            , "|  Corrupt File: " ++ Path.toString path
            , "|   Byte Offset: " ++ String.fromInt offset
            , "|       Message: " ++ message
            , "|"
            , "| Please report this to https://github.com/elm/compiler/issues"
            , "| Trying to continue anyway."
            , "+-------------------------------------------------------------------------------"
            , ""
            ]) <| \_ ->
          IO.return Nothing

    Nothing ->
      IO.return Nothing



-- WRITE UTF-8


writeUtf8 : FilePath -> String -> IO b c d e ()
writeUtf8 filePath contents =
  Platform.writeFile filePath <| Bytes.Encode.encode <| Bytes.Encode.string contents



-- READ UTF-8


readUtf8 : FilePath -> IO b c d e String
readUtf8 path =
  Platform.readFile path |> IO.fmap (Maybe.andThen bytesToString >> Maybe.withDefault "")


bytesToString : Bytes -> Maybe String
bytesToString bytes =
  Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes



-- WRITE BUILDER


writeBuilder : FilePath -> String -> IO b c d e ()
writeBuilder =
  writeUtf8



-- WRITE PACKAGE


writePackage : FilePath -> Zip.Zip -> IO b c d e ()
writePackage destination archive =
  case Zip.entries archive of
    [] ->
      IO.return ()

    entry::entries ->
      let root = String.length (Zip.Entry.path entry) in
      MList.sortOn Zip.Entry.path entries
        |> MList.mapM_ IO.return IO.bind (writeEntry destination root)


writeEntry : FilePath -> Int -> Zip.Entry.Entry -> IO b c d e ()
writeEntry destination root entry =
  let
    path = String.dropLeft root (Zip.Entry.path entry)
  in
  if String.startsWith "src/" path
    || path == "LICENSE"
    || path == "README.md"
    || path == "elm.json"
  then
    if String.endsWith "/" path
    then Platform.createDirectoryIfMissing (Path.combine destination (Path.fromString path))
    else
      case Zip.Entry.toBytes entry of
        Err _ ->
          IO.return ()

        Ok bytes ->
          Platform.writeFile (Path.combine destination (Path.fromString path)) bytes
  else
    IO.return ()



-- EXISTS


exists : FilePath -> IO b c d e Bool
exists path =
  Platform.doesFileExist path



-- REMOVE FILES


remove : FilePath -> IO b c d e ()
remove path =
  Platform.removeFile path