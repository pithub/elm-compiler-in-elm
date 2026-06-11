module Extra.Type.Lens exposing
    ( Lens
    , compose
    , modify
    )


type alias Lens whole part =
    { getter : whole -> part
    , setter : part -> whole -> whole
    }


modify : Lens a b -> (b -> b) -> a -> a
modify lens f a =
    lens.setter (f (lens.getter a)) a


compose : Lens a b -> Lens b c -> Lens a c
compose lensAb lensBc =
    { getter = \a -> lensBc.getter (lensAb.getter a)
    , setter = \c a -> lensAb.setter (lensBc.setter c (lensAb.getter a)) a
    }
