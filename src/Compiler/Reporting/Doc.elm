{- MANUALLY FORMATTED -}
module Compiler.Reporting.Doc exposing
  ( Doc
  , align, cat, empty, fillSep, hang
  , hcat, hsep, indent, sep, vcat
  , red, redS, cyan, cyanS, green, greenS, blue, blackS, yellow, yellowS
  , dullred, dullcyan, dullcyanS, dullyellow, dullyellowS
  , plain
  --
  , fromChars
  , fromName
  , fromVersion
  , fromPackage
  , fromInt
  --
  , toAnsi
  , toString
  , toLine
  --
  , encode
  --
  , stack
  , reflow
  , commaSep
  --
  , toSimpleNote
  , toFancyNote
  , toSimpleHint
  , toFancyHint
  --
  , link
  , fancyLink
  , reflowLink
  , makeLink
  , makeNakedLink
  --
  , args
  , ordinal
  , intToOrdinal
  , cycle
  --
  , d, da
  , fromPath
  , toClient
  )


import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Encode as E
import Compiler.Json.String as Json
import Elm.Error as Client
import Extra.Data.Pretty as P
import Extra.Platform.Handle as Handle exposing (Handle)
import Extra.System.IO exposing (IO)
import Extra.System.Path as Path exposing (FilePath)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe



-- EXPORTS FROM EXTRA.DATA.PRETTY


type alias Doc =
  P.Doc Client.Style


align : Doc -> Doc
align =
  P.align


blackS : String -> Doc
blackS =
  P.blackS


blue : String -> Doc
blue =
  P.blue


cat : TList (Doc) -> Doc
cat =
  P.cat


cyan : Doc -> Doc
cyan =
  P.cyan


cyanS : String -> Doc
cyanS =
  P.cyanS


dullcyan : Doc -> Doc
dullcyan =
  P.dullcyan


dullcyanS : String -> Doc
dullcyanS =
  P.dullcyanS


dullred : Doc -> Doc
dullred =
  P.dullred


dullyellow : Doc -> Doc
dullyellow =
  P.dullyellow


dullyellowS : String -> Doc
dullyellowS =
  P.dullyellowS


empty : Doc
empty =
  P.empty


fillSep : TList (Doc) -> Doc
fillSep =
  P.fillSep


green : Doc -> Doc
green =
  P.green


greenS : String -> Doc
greenS =
  P.greenS


hang : Int -> Doc -> Doc
hang =
  P.hang


hcat : TList (Doc) -> Doc
hcat =
  P.hcat


da : TList (Doc) -> Doc
da {- abbreviated alias -} =
  hcat


hsep : TList (Doc) -> Doc
hsep =
  P.hsep


indent : Int -> Doc -> Doc
indent =
  P.indent


plain : Doc -> Doc
plain =
  P.plain


red : Doc -> Doc
red =
  P.red


redS : String -> Doc
redS =
  P.redS


sep : TList (Doc) -> Doc
sep =
  P.sep


vcat : TList (Doc) -> Doc
vcat =
  P.vcat


yellow : Doc -> Doc
yellow =
  P.yellow


yellowS : String -> Doc
yellowS =
  P.yellowS



-- FROM


fromChars : String -> Doc
fromChars =
  P.text


d : String -> Doc
d {- abbreviated alias -} =
  fromChars


fromName : Name.Name -> Doc
fromName name =
  P.text name


fromVersion : V.Version -> Doc
fromVersion vsn =
  P.text (V.toChars vsn)


fromPackage : Pkg.Name -> Doc
fromPackage pkg =
  P.text (Pkg.toChars pkg)


fromPath : FilePath -> Doc
fromPath path =
  P.text (Path.toString path)


fromInt : Int -> Doc
fromInt n =
  P.text (String.fromInt n)



-- TO STRING


toAnsi : Handle s a -> Doc -> IO s a
toAnsi handle doc =
  Handle.hSendStr handle (chunksToAnsi (toClient doc))


toString : Doc -> String
toString doc =
  P.pretty 80 doc


toLine : Doc -> String
toLine doc =
  P.pretty (2 ^ 40) doc



-- TO ANSI


chunksToAnsi : TList Client.Chunk -> String
chunksToAnsi chunks =
  String.concat (MList.map chunkToAnsi chunks)


chunkToAnsi : Client.Chunk -> String
chunkToAnsi chunk =
  case chunk of
    Client.Unstyled text ->
      text

    Client.Styled style text ->
      styleToAnsi style text


styleToAnsi : Client.Style -> String -> String
styleToAnsi ({ bold, underline, color} as style) =
  case ( bold, underline, color ) of
    ( True, _   , _            ) -> unexpected "bold" style
    ( _   , True, Nothing      ) -> wrapWithAnsi "4"
    ( _   , True, _            ) -> unexpected "underline with color" style
    ( _   , _   , Nothing      ) -> identity
    ( _, _, Just Client.RED    ) -> wrapWithAnsi "91"
    ( _, _, Just Client.GREEN  ) -> wrapWithAnsi "92"
    ( _, _, Just Client.CYAN   ) -> wrapWithAnsi "96"
    ( _, _, Just Client.BLUE   ) -> wrapWithAnsi "94"
    ( _, _, Just Client.BLACK  ) -> wrapWithAnsi "90"
    ( _, _, Just Client.YELLOW ) -> wrapWithAnsi "93"
    ( _, _, Just Client.Cyan   ) -> wrapWithAnsi "36"
    ( _, _, Just Client.Red    ) -> wrapWithAnsi "31"
    ( _, _, Just Client.Yellow ) -> wrapWithAnsi "33"
    _ {- (_, _, _ ) -}           -> unexpected "color" style


wrapWithAnsi : String -> String -> String
wrapWithAnsi ansiCode text =
  "\u{001b}[" ++ ansiCode ++ "m" ++ text ++ "\u{001b}[0m"


unexpected : String -> Client.Style -> String -> String
unexpected message style text =
  let _ = Debug.log ("Doc style: unexpected " ++ message) (style, text) in
  text



-- TO CHUNKS


clientStyles : P.Styles Client.Style
clientStyles =
  { underline = Client.Style False True Nothing
  , red = Client.Style False False (Just Client.RED)
  , green = Client.Style False False (Just Client.GREEN)
  , cyan = Client.Style False False (Just Client.CYAN)
  , blue = Client.Style False False (Just Client.BLUE)
  , black = Client.Style False False (Just Client.BLACK)
  , yellow = Client.Style False False (Just Client.YELLOW)
  , dullcyan = Client.Style False False (Just Client.Cyan)
  , dullred = Client.Style False False (Just Client.Red)
  , dullyellow = Client.Style False False (Just Client.Yellow)
  }


toClient : Doc -> TList Client.Chunk
toClient doc =
  P.renderPretty 80 clientRenderer doc


clientRenderer : P.Renderer Client.Style (TList MultiStringChunk) (TList Client.Chunk)
clientRenderer =
  { init = clientRendererInit
  , tagged = clientRendererTagged
  , untagged = clientRendererUntagged
  , newline = clientRendererNewline
  , outer = clientRendererOuter
  }


type alias MultiStringChunk = ( Maybe Client.Style, TList String )


addStyledString : Maybe Client.Style -> String -> TList MultiStringChunk -> TList MultiStringChunk
addStyledString maybeStyle text chunks =
  case chunks of
    [] ->
      [ ( maybeStyle, [text] ) ]

    ( maybeHeadStyle, headStrings) :: tail ->
      if maybeHeadStyle == maybeStyle then
        ( maybeHeadStyle, text :: headStrings ) :: tail
      else
        ( maybeStyle, [text] ) :: chunks


clientRendererInit : TList MultiStringChunk
clientRendererInit =
  []


clientRendererTagged : (P.Styles Client.Style -> Client.Style) -> String -> TList MultiStringChunk -> TList MultiStringChunk
clientRendererTagged tagger text multiStringChunks =
  addStyledString (Just (tagger clientStyles)) text multiStringChunks


clientRendererUntagged : String -> TList MultiStringChunk -> TList MultiStringChunk
clientRendererUntagged text multiStringChunks =
  addStyledString Nothing text multiStringChunks


clientRendererNewline : TList MultiStringChunk -> TList MultiStringChunk
clientRendererNewline multiStringChunks =
  addStyledString Nothing "\n" multiStringChunks


clientRendererOuter : TList MultiStringChunk -> TList Client.Chunk
clientRendererOuter multiStringChunks =
  multiStringChunksToClientChunks multiStringChunks []


multiStringChunksToClientChunks : TList MultiStringChunk -> TList Client.Chunk -> TList Client.Chunk
multiStringChunksToClientChunks multiStringChunks clientChunks =
  case multiStringChunks of
    [] ->
      clientChunks

    ( maybeStyle, strings ) :: remainingMultiStringChunks ->
      let
        clientChunk =
          case maybeStyle of
            Nothing ->
              Client.Unstyled (String.concat (MList.reverse strings))

            Just style ->
              Client.Styled style (String.concat (MList.reverse strings))
      in
      multiStringChunksToClientChunks remainingMultiStringChunks (clientChunk :: clientChunks)



-- FORMATTING


stack : TList (Doc) -> Doc
stack docs =
  P.vcat (MList.intersperse (P.text "") docs)


reflow : String -> Doc
reflow paragraph =
  P.fillSep (MList.map P.text (String.words paragraph))


commaSep : Doc -> (a -> Doc) -> TList a -> TList (Doc)
commaSep conjunction addStyle names =
  case names of
    [name] ->
      [ addStyle name ]

    [name1, name2] ->
      [ addStyle name1, conjunction, addStyle name2 ]

    _ ->
      MList.map (\name -> hcat [ addStyle name, fromChars "," ]) (MList.init names)
      ++
      [ conjunction
      , addStyle (MList.last names)
      ]



-- NOTES


toSimpleNote : String -> Doc
toSimpleNote message =
  toFancyNote (MList.map P.text (String.words message))


toFancyNote : TList (Doc) -> Doc
toFancyNote chunks =
  P.fillSep (P.hcat [ P.underline "Note", P.text ":" ] :: chunks)



-- HINTS


toSimpleHint : String -> Doc
toSimpleHint message =
  toFancyHint (MList.map P.text (String.words message))


toFancyHint : TList (Doc) -> Doc
toFancyHint chunks =
  P.fillSep (P.hcat [ P.underline "Hint", P.text ":" ] :: chunks)



-- LINKS


link : String -> String -> String -> String -> Doc
link word before fileName after =
  P.fillSep <|
    P.hcat [ P.underline word, P.text ":" ]
    :: MList.map P.text (String.words before)
    ++ P.text (makeLink fileName)
    :: MList.map P.text (String.words after)


fancyLink : String -> TList (Doc) -> String -> TList (Doc) -> Doc
fancyLink word before fileName after =
  P.fillSep <|
    P.hcat [ P.underline word, P.text ":" ] :: before ++ P.text (makeLink fileName) :: after


makeLink : String -> String
makeLink fileName =
  "<https://elm-lang.org/" ++ V.toChars V.compiler ++ "/" ++ fileName ++ ">"


makeNakedLink : String -> String
makeNakedLink fileName =
  "https://elm-lang.org/" ++ V.toChars V.compiler ++ "/" ++ fileName


reflowLink : String -> String -> String -> Doc
reflowLink before fileName after =
  P.fillSep <|
    MList.map P.text (String.words before)
    ++ P.text (makeLink fileName)
    :: MList.map P.text (String.words after)



-- HELPERS


args : Int -> String
args n =
  String.fromInt n ++ (if n == 1 then " argument" else " arguments")


ordinal : Index.ZeroBased -> String
ordinal index =
  intToOrdinal (Index.toHuman index)


intToOrdinal : Int -> String
intToOrdinal number =
  let
    remainder10 =
      modBy 10 number

    remainder100 =
      modBy 100 number

    ending =
      if MList.elem remainder100 [ 11, 12, 13 ] then "th"
      else if remainder10 == 1                  then "st"
      else if remainder10 == 2                  then "nd"
      else if remainder10 == 3                  then "rd"
      else                                           "th"
  in
  String.fromInt number ++ ending


cycle : Int -> Name.Name -> TList Name.Name -> Doc
cycle indent_ name names =
  let
    toLn n = hcat [ cycleLn, dullyellowS n ]
  in
  P.indent indent_ <| P.vcat <|
    cycleTop :: MList.intersperse cycleMid (toLn name :: MList.map toLn names) ++ [ cycleEnd ]


cycleTop : Doc
cycleTop = P.text "┌─────┐"
cycleLn  : Doc
cycleLn  = P.text "│    "
cycleMid : Doc
cycleMid = P.text "│     ↓"
cycleEnd : Doc
cycleEnd = P.text "└─────┘"



-- JSON


encode : Doc -> E.Value
encode doc =
  E.array (toJsonHelp (toClient doc))


toJsonHelp : TList Client.Chunk -> TList E.Value
toJsonHelp chunks =
  MList.map encodeChunk chunks


encodeChunk : Client.Chunk -> E.Value
encodeChunk chunk =
  case chunk of
    Client.Unstyled text ->
      E.chars text

    Client.Styled { bold, underline, color } text ->
      E.object
        [ ("bold", E.bool bold)
        , ("underline", E.bool underline)
        , ("color", MMaybe.maybe E.null encodeColor color)
        , ("string", E.chars text)
        ]


encodeColor : Client.Color -> E.Value
encodeColor color =
  E.string <| Json.fromChars <|
    case color of
      Client.Red -> "red"
      Client.RED -> "RED"
      Client.Magenta -> "magenta"
      Client.MAGENTA -> "MAGENTA"
      Client.Yellow -> "yellow"
      Client.YELLOW -> "YELLOW"
      Client.Green -> "green"
      Client.GREEN -> "GREEN"
      Client.Cyan -> "cyan"
      Client.CYAN -> "CYAN"
      Client.Blue -> "blue"
      Client.BLUE -> "BLUE"
      Client.Black -> "black"
      Client.BLACK -> "BLACK"
      Client.White -> "white"
      Client.WHITE -> "WHITE"
