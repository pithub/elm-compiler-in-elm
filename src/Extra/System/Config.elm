module Extra.System.Config exposing
    ( GlobalState
    , LocalState
    , httpPrefix
    , initialState
    , mountPrefix
    , setHttpPrefix
    , setMountPrefix
    )

import Extra.System.IO as IO
import Extra.Type.Lens exposing (Lens)
import Global



-- PUBLIC STATE


type alias GlobalState b c d e f g h =
    Global.State LocalState b c d e f g h


type LocalState
    = LocalState
        -- httpPrefix
        (Maybe String)
        -- mountPrefix
        (Maybe String)


initialState : LocalState
initialState =
    LocalState
        -- httpPrefix
        Nothing
        -- mountPrefix
        Nothing


lensHttpPrefix : Lens (GlobalState b c d e f g h) (Maybe String)
lensHttpPrefix =
    { getter = \(Global.State (LocalState x _) _ _ _ _ _ _ _) -> x
    , setter = \x (Global.State (LocalState _ bi) b c d e f g h) -> Global.State (LocalState x bi) b c d e f g h
    }


lensMountPrefix : Lens (GlobalState b c d e f g h) (Maybe String)
lensMountPrefix =
    { getter = \(Global.State (LocalState _ x) _ _ _ _ _ _ _) -> x
    , setter = \x (Global.State (LocalState ai _) b c d e f g h) -> Global.State (LocalState ai x) b c d e f g h
    }



-- PRIVATE IO


type alias IO b c d e f g h v =
    IO.IO (GlobalState b c d e f g h) v



-- ACCESSORS


httpPrefix : IO b c d e f g h (Maybe String)
httpPrefix =
    IO.getLens lensHttpPrefix


setHttpPrefix : Maybe String -> IO b c d e f g h ()
setHttpPrefix prefix =
    IO.putLens lensHttpPrefix prefix


mountPrefix : IO b c d e f g h (Maybe String)
mountPrefix =
    IO.getLens lensMountPrefix


setMountPrefix : Maybe String -> IO b c d e f g h ()
setMountPrefix prefix =
    IO.putLens lensMountPrefix prefix
