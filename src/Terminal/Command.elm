module Terminal.Command exposing
    ( LocalState
    , Output
    , State
    , ask
    , clearInput
    , clearPrompt
    , clearPutLine
    , clearStdOut
    , getDurationSinceLastInput
    , getInput
    , getLine
    , getLineWithInitial
    , getText
    , gotInput
    , gotLine
    , initialState
    , lensInput
    , lensPrompt
    , lensStdIn
    , lensStdOut
    , putDoc
    , putLine
    , putTemporary
    , setCurrentInput
    , setInput
    , setNextInput
    )

import Builder.Generate as Generate
import Compiler.Reporting.Doc as D
import Extra.System.IO as IO
import Extra.Type.Lens exposing (Lens)
import Extra.Type.List exposing (TList)
import Global
import Time



-- PUBLIC STATE


type alias State f g h =
    Generate.State (LocalState f g h) f g h


type LocalState f g h
    = LocalState
        -- stdIn
        (TList (String -> IO f g h ()))
        -- stdOut
        (TList Output)
        -- prompt
        String
        -- input
        String
        -- waiting
        (Maybe String)
        -- inputTime
        (Maybe Int)


initialState : LocalState f g h
initialState =
    LocalState
        -- stdIn
        []
        -- stdOut
        []
        -- prompt
        ""
        -- input
        ""
        -- waiting
        Nothing
        -- inputTime
        Nothing


lensStdIn : Lens (State f g h) (TList (String -> IO f g h ()))
lensStdIn =
    { getter = \(Global.State _ _ _ _ (LocalState x _ _ _ _ _) _ _ _) -> x
    , setter = \x (Global.State a b c d (LocalState _ bi ci di ei fi) f g h) -> Global.State a b c d (LocalState x bi ci di ei fi) f g h
    }


lensStdOut : Lens (State f g h) (TList Output)
lensStdOut =
    { getter = \(Global.State _ _ _ _ (LocalState _ x _ _ _ _) _ _ _) -> x
    , setter = \x (Global.State a b c d (LocalState ai _ ci di ei fi) f g h) -> Global.State a b c d (LocalState ai x ci di ei fi) f g h
    }


lensPrompt : Lens (State f g h) String
lensPrompt =
    { getter = \(Global.State _ _ _ _ (LocalState _ _ x _ _ _) _ _ _) -> x
    , setter = \x (Global.State a b c d (LocalState ai bi _ di ei fi) f g h) -> Global.State a b c d (LocalState ai bi x di ei fi) f g h
    }


lensInput : Lens (State f g h) String
lensInput =
    { getter = \(Global.State _ _ _ _ (LocalState _ _ _ x _ _) _ _ _) -> x
    , setter = \x (Global.State a b c d (LocalState ai bi ci _ ei fi) f g h) -> Global.State a b c d (LocalState ai bi ci x ei fi) f g h
    }


lensWaiting : Lens (State f g h) (Maybe String)
lensWaiting =
    { getter = \(Global.State _ _ _ _ (LocalState _ _ _ _ x _) _ _ _) -> x
    , setter = \x (Global.State a b c d (LocalState ai bi ci di _ fi) f g h) -> Global.State a b c d (LocalState ai bi ci di x fi) f g h
    }


lensInputTime : Lens (State f g h) (Maybe Int)
lensInputTime =
    { getter = \(Global.State _ _ _ _ (LocalState _ _ _ _ _ x) _ _ _) -> x
    , setter = \x (Global.State a b c d (LocalState ai bi ci di ei _) f g h) -> Global.State a b c d (LocalState ai bi ci di ei x) f g h
    }



-- PRIVATE IO


type alias IO f g h v =
    IO.IO (State f g h) v



-- STDIN


getLine : IO f g h String
getLine =
    getLineWithInitial "" ""


getLineWithInitial : String -> String -> IO f g h String
getLineWithInitial prompt prefill =
    IO.bindSequence
        [ IO.putLens lensPrompt prompt
        , IO.putLens lensInput prefill
        ]
        (IO.liftCont <|
            \cont ->
                IO.bind (IO.getLens lensWaiting) <|
                    \waiting ->
                        case waiting of
                            Just line ->
                                IO.bindSequence
                                    [ IO.putLens lensWaiting Nothing ]
                                    (cont line)

                            Nothing ->
                                IO.modifyLens lensStdIn <| \cs -> cont :: cs
        )


gotLine : String -> IO f g h ()
gotLine line =
    IO.bind (IO.getLens lensStdIn) <|
        \stdIn ->
            case stdIn of
                [] ->
                    IO.noOp

                continuation :: cs ->
                    IO.bindSequence
                        [ IO.putLens lensStdIn cs
                        , setInputTime
                        ]
                        (continuation line)



-- STDOUT


type Output
    = Permanent String
    | Temporary String


getText : Output -> String
getText output =
    case output of
        Permanent string ->
            string

        Temporary string ->
            string


putLine : String -> IO f g h ()
putLine line =
    putOutput <| Permanent line


putTemporary : String -> IO f g h ()
putTemporary line =
    putOutput <| Temporary line


putDoc : D.Doc -> IO f g h ()
putDoc doc =
    putOutput <| Permanent (D.toString doc)


putOutput : Output -> IO f g h ()
putOutput output =
    IO.modifyLens lensStdOut <|
        \stdOut ->
            case stdOut of
                (Temporary _) :: rest ->
                    output :: rest

                _ ->
                    output :: stdOut


clearStdOut : IO f g h ()
clearStdOut =
    IO.putLens lensStdOut []


clearPutLine : String -> IO f g h ()
clearPutLine string =
    IO.sequence
        [ clearStdOut
        , putLine string
        ]



-- PROMPT


clearPrompt : IO f g h ()
clearPrompt =
    IO.putLens lensPrompt ""



-- ASK


ask : D.Doc -> IO f g h Bool
ask doc =
    IO.bind clearInput <|
        \_ ->
            IO.bind (putDoc doc) <|
                \_ ->
                    askHelp


askHelp : IO f g h Bool
askHelp =
    IO.bind (getLineWithInitial "?\u{2000}" "") <|
        \input ->
            IO.bind clearInput <|
                \_ ->
                    case input of
                        "" ->
                            IO.return True

                        "Y" ->
                            IO.return True

                        "y" ->
                            IO.return True

                        "n" ->
                            IO.bindSequence
                                [ clearInputTime ]
                                (IO.return False)

                        _ ->
                            IO.bind (putTemporary "Must type 'y' for yes or 'n' for no: ") <|
                                \_ ->
                                    askHelp



-- INPUT


clearInput : IO f g h ()
clearInput =
    setInput ""


setInput : String -> IO f g h ()
setInput input =
    IO.putLens lensInput input


getInput : IO f g h String
getInput =
    IO.getLens lensInput


gotInput : IO f g h ()
gotInput =
    IO.bind (IO.getLens lensInput) gotLine



-- WAITING


setCurrentInput : String -> IO f g h ()
setCurrentInput input =
    gotLine input


setNextInput : String -> IO f g h ()
setNextInput input =
    IO.putLens lensWaiting (Just input)



-- INPUT TIME


clearInputTime : IO f g h ()
clearInputTime =
    IO.putLens lensInputTime Nothing


setInputTime : IO f g h ()
setInputTime =
    IO.bind IO.now <|
        \now ->
            IO.putLens lensInputTime (Just (Time.posixToMillis now))


getDurationSinceLastInput : IO f g h (Maybe Int)
getDurationSinceLastInput =
    IO.bind (IO.getLens lensInputTime) <|
        \inputTime ->
            case inputTime of
                Just lastInputTime ->
                    IO.bind IO.now <|
                        \now ->
                            IO.return (Just (Time.posixToMillis now - lastInputTime))

                Nothing ->
                    IO.return Nothing
