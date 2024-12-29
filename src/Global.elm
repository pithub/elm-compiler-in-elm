module Global exposing (State(..))


type State a b c d e f g h
    = State
        -- SysFile
        a
        -- Details
        b
        -- Build
        c
        -- Generate
        d
        -- Terminal
        e
        -- Repl
        f
        -- Reactor
        g
        -- App1
        h
