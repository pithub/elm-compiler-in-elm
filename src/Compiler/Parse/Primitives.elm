{- MANUALLY FORMATTED -}
module Compiler.Parse.Primitives exposing
  ( fromByteString
  , Parser(..)
  , State(..)
  , Row
  , Col
  , oneOf, oneOfWithFallback
  , inContext, specialize
  , getPosition, addLocation, addEnd
  , withIndent, withBacksetIndent
  , word1, word2
  , unsafeIndex, isWord, getCharWidth
  , Snippet(..)
  , fromSnippet
  --
  , fmap
  , return, bind
  )


import Compiler.Reporting.Annotation as A
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List exposing (TList)



-- PARSER


type Parser z x a =
  Parser (
      State
      -> (a -> State -> z)                       -- consumed ok
      -> (a -> State -> z)                       -- empty ok
      -> (Row -> Col -> (Row -> Col -> x) -> z)  -- consumed err
      -> (Row -> Col -> (Row -> Col -> x) -> z)  -- empty err
      -> z
  )


type State = -- PERF try taking some out to avoid allocation
  State
    {- src -} String
    {- pos -} Int
    {- end -} Int
    {- indent -} Int
    {- row -} Row
    {- col -} Col


type alias Row = Int
type alias Col = Int



-- FUNCTOR


fmap : Functor.Fmap a (Parser z x a) b (Parser z x b)
fmap f (Parser parser) =
  Parser <| \state cok eok cerr eerr ->
    let
      cok_ a s = cok (f a) s
      eok_ a s = eok (f a) s
    in
    parser state cok_ eok_ cerr eerr



-- ONE OF


oneOf : (Row -> Col -> x) -> TList (Parser z x a) -> Parser z x a
oneOf toError parsers =
  Parser <| \state cok eok cerr eerr ->
    oneOfHelp state cok eok cerr eerr toError parsers


oneOfHelp
  : State
  -> (a -> State -> z)
  -> (a -> State -> z)
  -> (Row -> Col -> (Row -> Col -> x) -> z)
  -> (Row -> Col -> (Row -> Col -> x) -> z)
  -> (Row -> Col -> x)
  -> TList (Parser z x a)
  -> z
oneOfHelp state cok eok cerr eerr toError parsers =
  case parsers of
    Parser parser :: parsers_ ->
      let
        eerr_ _ _ _ =
          oneOfHelp state cok eok cerr eerr toError parsers_
      in
      parser state cok eok cerr eerr_

    [] ->
      let
        (State _ _ _ _ row col) = state
      in
      eerr row col toError



-- ONE OF WITH FALLBACK


oneOfWithFallback : TList (Parser z x a) -> a -> Parser z x a -- PERF is this function okay? Worried about allocation/laziness with fallback values.
oneOfWithFallback parsers fallback =
  Parser <| \state cok eok cerr _ ->
    oowfHelp state cok eok cerr parsers fallback


oowfHelp
  : State
  -> (a -> State -> z)
  -> (a -> State -> z)
  -> (Row -> Col -> (Row -> Col -> x) -> z)
  -> TList (Parser z x a)
  -> a
  -> z
oowfHelp state cok eok cerr parsers fallback =
  case parsers of
    [] ->
      eok fallback state

    Parser parser :: parsers_ ->
      let
        eerr_ _ _ _ =
          oowfHelp state cok eok cerr parsers_ fallback
      in
      parser state cok eok cerr eerr_



-- MONAD


return : Monad.Return a (Parser z x a)
return value =
  Parser <| \state _ eok _ _ ->
    eok value state


bind : Monad.Bind a (Parser z x a) (Parser z x b)
bind (Parser parserA) callback =
  Parser <| \state cok eok cerr eerr ->
    let
      cok_ a s =
        case callback a of
          Parser parserB -> parserB s cok cok cerr cerr

      eok_ a s =
        case callback a of
          Parser parserB -> parserB s cok eok cerr eerr
    in
    parserA state cok_ eok_ cerr eerr



-- FROM BYTESTRING


fromByteString : Parser (Either x a) x a -> (Row -> Col -> x) -> String -> Either x a
fromByteString (Parser parser) toBadEnd fptr =
  let
    toOk_ = toOk toBadEnd
    pos = 0
    end = String.length fptr
    result = parser (State fptr pos end 0 1 1) toOk_ toOk_ toErr toErr
  in
  result


toOk : (Row -> Col -> x) -> a -> State -> Either x a
toOk toBadEnd a (State _ pos end _ row col) =
  if pos == end
  then Right a
  else Left (toBadEnd row col)


toErr : Row -> Col -> (Row -> Col -> x) -> Either x a
toErr row col toError =
  Left (toError row col)



-- FROM SNIPPET


type Snippet =
  Snippet
    {- fptr   -} String
    {- offset -} Int
    {- length -} Int
    {- offRow -} Row
    {- offCol -} Col


fromSnippet : Parser (Either x a) x a -> (Row -> Col -> x) -> Snippet -> Either x a
fromSnippet (Parser parser) toBadEnd (Snippet fptr offset length row col) =
  let
    toOk_ = toOk toBadEnd
    pos = offset
    end = pos + length
    result = parser (State fptr pos end 0 row col) toOk_ toOk_ toErr toErr
  in
  result



-- POSITION


getPosition : Parser z x A.Position
getPosition =
  Parser <| \((State _ _ _ _ row col) as state) _ eok _ _ ->
    eok (A.Position row col) state


addLocation : Parser z x a -> Parser z x (A.Located a)
addLocation (Parser parser) =
  Parser <| \((State _ _ _ _ sr sc) as state) cok eok cerr eerr ->
    let
      cok_ a ((State _ _ _ _ er ec) as s) = cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) a) s
      eok_ a ((State _ _ _ _ er ec) as s) = eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) a) s
    in
    parser state cok_ eok_ cerr eerr


addEnd : A.Position -> a -> Parser z x (A.Located a)
addEnd start value =
  Parser <| \((State _ _ _ _ row col) as state) _ eok _ _ ->
    eok (A.at start (A.Position row col) value) state



-- INDENT


withIndent : Parser z x a -> Parser z x a
withIndent (Parser parser) =
  Parser <| \(State src pos end oldIndent row col) cok eok cerr eerr ->
    let
      cok_ a (State s p e _ r c) = cok a (State s p e oldIndent r c)
      eok_ a (State s p e _ r c) = eok a (State s p e oldIndent r c)
    in
    parser (State src pos end col row col) cok_ eok_ cerr eerr


withBacksetIndent : Int -> Parser z x a -> Parser z x a
withBacksetIndent backset (Parser parser) =
  Parser <| \(State src pos end oldIndent row col) cok eok cerr eerr ->
    let
      cok_ a (State s p e _ r c) = cok a (State s p e oldIndent r c)
      eok_ a (State s p e _ r c) = eok a (State s p e oldIndent r c)
    in
    parser (State src pos end (col - backset) row col) cok_ eok_ cerr eerr



-- CONTEXT


inContext : (x -> Row -> Col -> y) -> Parser z y start -> Parser z x a -> Parser z y a
inContext addContext (Parser parserStart) (Parser parserA) =
  Parser <| \((State _ _ _ _ row col) as state) cok eok cerr eerr ->
    let
      cerrA r c tx = cerr row col (addContext (tx r c))
      eerrA r c tx = eerr row col (addContext (tx r c))

      cokS _ s = parserA s cok cok cerrA cerrA
      eokS _ s = parserA s cok eok cerrA eerrA
    in
    parserStart state cokS eokS cerr eerr


specialize : (x -> Row -> Col -> y) -> Parser z x a -> Parser z y a
specialize addContext (Parser parser) =
  Parser <| \((State _ _ _ _ row col) as state) cok eok cerr eerr ->
    let
      cerr_ r c tx = cerr row col (addContext (tx r c))
      eerr_ r c tx = eerr row col (addContext (tx r c))
    in
    parser state cok eok cerr_ eerr_



-- SYMBOLS


word1 : Int -> (Row -> Col -> x) -> Parser z x ()
word1 word toError =
  Parser <| \(State src pos end indent row col) cok _ _ eerr ->
    if pos < end && unsafeIndex src pos == word then
      let newState = State src (pos + 1) end indent row (col + 1) in
      cok () newState
    else
      eerr row col toError


word2 : Int -> Int -> (Row -> Col -> x) -> Parser z x ()
word2 w1 w2 toError =
  Parser <| \(State src pos end indent row col) cok _ _ eerr ->
    let
      pos1 = pos + 1
    in
    if pos1 < end && unsafeIndex src pos == w1 && unsafeIndex src pos1 == w2 then
      let newState = State src (pos + 2) end indent row (col + 2) in
      cok () newState
    else
      eerr row col toError



-- LOW-LEVEL CHECKS


unsafeIndex : String -> Int -> Int
unsafeIndex src pos =
  src
    |> String.dropLeft pos
    |> String.uncons
    |> Maybe.map (Tuple.first >> Char.toCode)
    |> Maybe.withDefault 0


isWord : String -> Int -> Int -> Int -> Bool
isWord src pos end word =
  pos < end && unsafeIndex src pos == word


getCharWidth : Int -> Int
getCharWidth word =
  if word > 0xFFFF then 2
  else 1
