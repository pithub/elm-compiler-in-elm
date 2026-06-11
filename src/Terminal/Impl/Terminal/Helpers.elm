{- MANUALLY FORMATTED -}
module Terminal.Impl.Terminal.Helpers exposing
  ( version
  , elmFile
  , package
  )


import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Parse.Primitives as P
import Extra.System.IO as IO exposing (IO)
import Extra.System.Path as Path exposing (FilePath)
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Terminal.Impl.Terminal.Internal as Internal



-- VERSION


version : Internal.Parser s V.Version
version =
  Internal.Parser
    {- singular -} "version"
    {- plural -} "versions"
    {- parser -} parseVersion
    {- suggest -} suggestVersion
    {- examples -} (IO.return << exampleVersions)


parseVersion : String -> Maybe V.Version
parseVersion chars =
  case P.fromByteString V.parser Tuple.pair chars of
    Right vsn -> Just vsn
    Left _    -> Nothing


suggestVersion : String -> IO s (TList String)
suggestVersion _ =
  IO.return []


exampleVersions : String -> TList String
exampleVersions chars =
  let
    chunks = String.split "." chars
    isNumber cs = not (String.isEmpty cs) && String.all Char.isDigit cs
  in
  if MList.all isNumber chunks then
    case chunks of
      [x]        -> [ x ++ ".0.0" ]
      [x,y]      -> [ x ++ "." ++ y ++ ".0" ]
      x::y::z::_ -> [ x ++ "." ++ y ++ "." ++ z ]
      _          -> ["1.0.0", "2.0.3"]

  else
    ["1.0.0", "2.0.3"]



-- ELM FILE


elmFile : Internal.Parser s FilePath
elmFile =
  Internal.Parser
    {- singular -} "elm file"
    {- plural -} "elm files"
    {- parser -} parseElmFile
    {- suggest -} (\_ -> IO.return [])
    {- examples -} exampleElmFiles


parseElmFile : String -> Maybe FilePath
parseElmFile chars =
  if String.endsWith ".elm" chars then
    Just (Path.fromString chars)
  else
    Nothing


exampleElmFiles : String -> IO s (TList String)
exampleElmFiles _ =
  IO.return ["Main.elm","src/Main.elm"]



-- PACKAGE


package : Internal.Parser s Pkg.Name
package =
  Internal.Parser
    {- singular -} "package"
    {- plural -} "packages"
    {- parser -} parsePackage
    {- suggest -} suggestPackages
    {- examples -} examplePackages


parsePackage : String -> Maybe Pkg.Name
parsePackage chars =
  case P.fromByteString Pkg.parser Tuple.pair chars of
    Right pkg -> Just pkg
    Left _    -> Nothing


suggestPackages : String -> IO s (TList String)
suggestPackages _ =
  IO.return []


examplePackages : String -> IO s (TList String)
examplePackages _ =
  IO.return
    [ "elm/json"
    , "elm/http"
    , "elm/random"
    ]
