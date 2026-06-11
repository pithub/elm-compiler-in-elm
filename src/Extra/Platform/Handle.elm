module Extra.Platform.Handle exposing
    ( Handle
    , hIsTerminalDevice
    , hPutStr
    , hSendStr
    , stdIn
    , stderr
    , stdout
    )

import Extra.Platform as Platform
import Extra.System.IO as IO exposing (IO)


type alias GlobalState b c d e =
    Platform.GlobalState b c d e


type Handle s v
    = Handle (String -> IO s v) (IO s Bool)


stdIn : Handle (GlobalState b c d e) String
stdIn =
    Handle (\prompt -> Platform.consoleRead prompt "") (IO.return False)


stdout : Handle (GlobalState b c d e) ()
stdout =
    Handle Platform.consoleWrite Platform.stdOutIsTty


stderr : Handle (GlobalState b c d e) ()
stderr =
    Handle Platform.consoleError Platform.stdErrIsTty


hPutStr : Handle s () -> String -> IO s ()
hPutStr =
    hSendStr


hSendStr : Handle s v -> String -> IO s v
hSendStr (Handle consoleFun _) =
    consoleFun


hIsTerminalDevice : Handle s () -> IO s Bool
hIsTerminalDevice (Handle _ isTty) =
    isTty
