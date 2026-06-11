{- MANUALLY FORMATTED -}
module Terminal.Develop exposing
  ( Flags(..)
  , run
  )


import Builder.Generate as Generate
import Extra.Platform as Platform



-- IO


type alias IO e v =
  Generate.IO e v



-- RUN THE DEV SERVER


type Flags =
  Flags
    {- port -} (Maybe Int)


run : () -> Flags -> IO e ()
run () _ =
  Platform.consoleError "elm reactor: not yet implemented"
