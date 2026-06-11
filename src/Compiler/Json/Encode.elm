{- MANUALLY FORMATTED -}
module Compiler.Json.Encode exposing
  ( write
  , encodeUgly
  , Value
  , array
  , object
  , string
  , name
  , path
  , chars
  , bool
  , int
  , null
  , dict
  , list
  )


import Builder.File as File
import Compiler.Data.Name as Name
import Compiler.Json.String as Json
import Extra.Platform as Platform
import Extra.System.Path as Path exposing (FilePath)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- IO


type alias IO b c d e v =
  Platform.IO b c d e v



-- VALUES


type Value
  = Array (TList Value)
  | Object (TList (Json.TString, Value))
  | CString String
  | Boolean Bool
  | Integer Int
  | Null


array : TList Value -> Value
array =
  Array


object : TList (Json.TString, Value) -> Value
object =
  Object


string : Json.TString -> Value
string str =
  CString ("\"" ++ Json.toBuilder str ++ "\"")


name : Name.Name -> Value
name nm =
  CString ("\"" ++ Name.toBuilder nm ++ "\"")


path : FilePath -> Value
path filePath =
  CString ("\"" ++ Json.toBuilder (Path.toString filePath) ++ "\"")


bool : Bool -> Value
bool =
  Boolean


int : Int -> Value
int =
  Integer


null : Value
null =
  Null


dict : (comparable -> Json.TString) -> (v -> Value) -> Map.Map comparable v -> Value
dict encodeKey encodeValue pairs =
  Object <| MList.map (\( k, v ) -> ( encodeKey k, encodeValue v )) (Map.toList pairs)


list : (a -> Value) -> TList a -> Value
list encodeEntry entries =
  Array <| MList.map encodeEntry entries



-- CHARS


chars : String -> Value
chars chrs =
  -- PERF can this be done better? Look for examples.
  CString ("\"" ++ escape chrs ++ "\"")


escape : String -> String
escape chrs =
  case String.uncons chrs of
    Nothing ->
      ""

    Just ( c, cs ) ->
      if c == '\r' then String.cons '\\' (String.cons 'r'  (escape cs))
      else if c == '\n' then String.cons '\\' (String.cons 'n'  (escape cs))
      else if c == '\"' then String.cons '\\' (String.cons '"'  (escape cs))
      else if c == '\\' then String.cons '\\' (String.cons '\\' (escape cs))
      else String.cons c (escape cs)



-- WRITE TO FILE


write : FilePath -> Value -> IO b c d e ()
write filePath value =
  File.writeBuilder filePath (encode value ++ "\n")



-- ENCODE UGLY


encodeUgly : Value -> String
encodeUgly value =
  case value of
    Array [] ->
      "[]"

    Array (first :: rest) ->
      let
        encodeEntry entry =
          "," ++ encodeUgly entry
      in
        "[" ++ encodeUgly first ++ String.concat (MList.map encodeEntry rest) ++ "]"

    Object [] ->
      "{}"

    Object (first :: rest) ->
      let
        encodeEntry char (key, entry) =
          String.fromChar char ++ "\"" ++ key ++ "\":" ++ encodeUgly entry
      in
        encodeEntry '{' first ++ String.concat (MList.map (encodeEntry ',') rest) ++ "}"

    CString builder ->
      builder

    Boolean boolean ->
      if boolean then "true" else "false"

    Integer n ->
      String.fromInt n

    Null ->
      "null"



-- ENCODE


encode : Value -> String
encode value =
  encodeHelp "" value


encodeHelp : String -> Value -> String
encodeHelp indent value =
  case value of
    Array [] ->
      "[]"

    Array (first :: rest) ->
      encodeArray indent first rest

    Object [] ->
      "{}"

    Object (first :: rest) ->
      encodeObject indent first rest

    CString builder ->
      builder

    Boolean boolean ->
      if boolean then "true" else "false"

    Integer n ->
      String.fromInt n

    Null ->
      "null"



-- ENCODE ARRAY


encodeArray : String -> Value -> TList Value -> String
encodeArray =
  encodeSequence arrayOpen arrayClose encodeHelp


arrayOpen : String
arrayOpen =
  "[\n"


arrayClose : String
arrayClose =
  "]"



-- ENCODE OBJECT


encodeObject : String -> (Json.TString, Value) -> (TList (Json.TString, Value)) -> String
encodeObject =
  encodeSequence objectOpen objectClose encodeField


objectOpen : String
objectOpen =
  "{\n"


objectClose : String
objectClose =
  "}"


encodeField : String -> (Json.TString, Value) -> String
encodeField indent (key, value) =
  "\"" ++ key ++ "\": " ++ encodeHelp indent value



-- ENCODE SEQUENCE


encodeSequence : String -> String -> (String -> a -> String) -> String -> a -> TList a -> String
encodeSequence open close encodeEntry indent first rest =
  let
    newIndent =
      indent ++ "    "

    newIndentBuilder =
      newIndent

    closer =
      newline ++ indent ++ close

    addValue field builder =
      commaNewline
      ++ newIndentBuilder
      ++ encodeEntry newIndent field
      ++ builder
  in
    open
    ++ newIndentBuilder
    ++ encodeEntry newIndent first
    ++ MList.foldr addValue closer rest


commaNewline : String
commaNewline =
  ",\n"


newline : String
newline =
  "\n"
