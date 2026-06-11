{- MANUALLY FORMATTED -}
module Terminal.Bump exposing
  ( run
  )


import Extra.Platform as Platform



-- IO


type alias IO b c d e v =
  Platform.IO b c d e v



-- RUN


run : () -> () -> IO b c d e ()
run () () =
  Platform.consoleError "elm bump: not yet implemented"
