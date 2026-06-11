{- MANUALLY FORMATTED -}
module Terminal.Publish exposing
  ( run
  )


import Extra.Platform as Platform



-- IO


type alias IO b c d e v =
  Platform.IO b c d e v



-- RUN


-- TODO mandate no "exposing (..)" in packages to make
-- optimization to skip builds in Elm.Details always valid


run : () -> () -> IO b c d e ()
run () () =
  Platform.consoleError "elm publish: not yet implemented"
