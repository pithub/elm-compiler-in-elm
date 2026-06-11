module Global exposing (State(..))


type State a b c d e
    = State
        -- Platform
        a
        -- Details
        b
        -- Build
        c
        -- Generate
        d
        -- App
        e
