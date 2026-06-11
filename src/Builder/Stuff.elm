{- MANUALLY FORMATTED -}
module Builder.Stuff exposing
  ( details
  , interfaces
  , objects
  , elmi
  , elmo
  , findRoot
  , PackageCache
  , getPackageCache
  , registry
  , package
  , getReplCache
  , getElmHome
  )


import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Extra.Platform as Platform
import Extra.System.IO as IO
import Extra.System.Path as Path exposing (FileName, FilePath)



-- IO


type alias IO b c d e v =
  Platform.IO b c d e v



-- PATHS


stuff : FilePath -> FilePath
stuff root =
  Path.addNames root [ "elm-stuff", compilerVersion ]


details : FilePath -> FilePath
details root =
  Path.addName (stuff root) "d.dat"


interfaces : FilePath -> FilePath
interfaces root =
  Path.addName (stuff root) "i.dat"


objects : FilePath -> FilePath
objects root =
  Path.addName (stuff root) "o.dat"


compilerVersion : FileName
compilerVersion =
  V.toChars V.compiler



-- ELMI and ELMO


elmi : FilePath -> ModuleName.Raw -> FilePath
elmi root name =
  toArtifactPath root name "elmi"


elmo : FilePath -> ModuleName.Raw -> FilePath
elmo root name =
  toArtifactPath root name "elmo"


toArtifactPath : FilePath -> ModuleName.Raw -> String -> FilePath
toArtifactPath root name ext =
  Path.addName (stuff root) (ModuleName.toHyphenName name ++ "." ++ ext)



-- ROOT


findRoot : IO b c d e (Maybe FilePath)
findRoot =
  IO.bind Platform.getCurrentDirectory <| \dir ->
  findRootHelp dir


findRootHelp : FilePath -> IO b c d e (Maybe FilePath)
findRootHelp dirs =
  IO.bind (Platform.doesFileExist (Path.addName dirs "elm.json")) <| \exists ->
  if exists
    then IO.return (Just dirs)
    else
      case Path.splitLastName dirs of
        ( _, "" ) ->
          IO.return Nothing

        ( parent, _ ) ->
          findRootHelp parent



-- PACKAGE CACHES


type PackageCache = PackageCache FilePath


getPackageCache : IO b c d e PackageCache
getPackageCache =
  IO.fmap PackageCache <| getCacheDir "packages"


registry : PackageCache -> FilePath
registry (PackageCache dir) =
  Path.addName dir "registry.dat"


package : PackageCache -> Pkg.Name -> V.Version -> FilePath
package (PackageCache dir) name version =
  Path.addName (Path.combine dir (Pkg.toFilePath name)) (V.toChars version)



-- CACHE


getReplCache : IO b c d e FilePath
getReplCache =
  getCacheDir "repl"


getCacheDir : FileName -> IO b c d e FilePath
getCacheDir projectName =
  IO.bind getElmHome <| \home ->
  let root = Path.addNames home [ compilerVersion, projectName ] in
  IO.bind (Platform.createDirectoryIfMissing root) <| \_ ->
  IO.return root


getElmHome : IO b c d e FilePath
getElmHome =
  Platform.getElmHome
