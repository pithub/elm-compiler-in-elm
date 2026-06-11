{- MANUALLY FORMATTED -}
module Terminal.Diff exposing
  ( Args(..)
  , run
  )


import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Extra.Platform as Platform



-- IO


type alias IO b c d e v =
  Platform.IO b c d e v



-- RUN


type Args
  = CodeVsLatest
  | CodeVsExactly V.Version
  | LocalInquiry V.Version V.Version
  | GlobalInquiry Pkg.Name V.Version V.Version


run : Args -> () -> IO b c d e ()
run _ () =
  Platform.consoleError "elm diff: not yet implemented"
