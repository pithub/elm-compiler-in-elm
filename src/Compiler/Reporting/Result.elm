{- MANUALLY FORMATTED -}
module Compiler.Reporting.Result exposing
  ( TResult(..), RStep(..)
  , run
  , ok
  , warn
  , throw
  --
  , fmap
  , pure, andMap, liftA2, discardFirst
  , return, bind, andThen
  , foldM, traverseList, sequenceAMap, traverseMap, traverseWithKey
  )


import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Reporting.Warning as Warning
import Extra.Class.Applicative as Applicative
import Extra.Class.Foldable as Foldable
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Class.Traversable as Traversable
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- RESULT


type TResult info warnings error a =
  CResult (info -> warnings -> RStep info warnings error a)


type RStep info warnings error a
  = Rbad info warnings (OneOrMore.OneOrMore error)
  | Rgood info warnings a


run : TResult () (TList w) e a -> (TList w, Either (OneOrMore.OneOrMore e) a)
run (CResult k) =
  case k () [] of
    Rbad () w e -> (MList.reverse w, Left e)
    Rgood () w a -> (MList.reverse w, Right a)



-- HELPERS


ok : a -> TResult i w e a
ok a =
  CResult <| \i w ->
    Rgood i w a


warn : Warning.Warning -> TResult i (TList Warning.Warning) e ()
warn warning =
  CResult <| \i warnings ->
    Rgood i (warning::warnings) ()


throw : e -> TResult i w e a
throw e =
  CResult <| \i w ->
    Rbad i w (OneOrMore.one e)



-- FANCY INSTANCE STUFF


fmap : Functor.Fmap a (TResult i w e a) b (TResult i w e b)
fmap func (CResult k) =
  CResult <| \i w ->
    case k i w of
      Rbad i1 w1 e ->
        Rbad i1 w1 e
      Rgood i1 w1 value ->
        Rgood i1 w1 (func value)


pure : Applicative.Pure a (TResult i w e a)
pure = ok


andMap : Applicative.AndMap (TResult i w e a) (TResult i w e (a -> b)) (TResult i w e b)
andMap (CResult kv) (CResult kf) =
  CResult <| \i w ->
    case kf i w of
      Rbad i1 w1 e1 ->
        case kv i1 w1 of
          Rbad i2 w2 e2 -> Rbad i2 w2 (OneOrMore.more e1 e2)
          Rgood i2 w2 _ -> Rbad i2 w2 e1

      Rgood i1 w1 func ->
        case kv i1 w1 of
          Rbad i2 w2 e2 -> Rbad i2 w2 e2
          Rgood i2 w2 value -> Rgood i2 w2 (func value)


liftA2 : Applicative.LiftA2 a (TResult i w e a) b (TResult i w e b) c (TResult i w e c)
liftA2 =
  Applicative.liftA2 fmap andMap


discardFirst : Applicative.DiscardFirst (TResult i w e a) (TResult i w e b)
discardFirst =
  Applicative.discardFirst liftA2


return : Monad.Return a (TResult i w e a)
return = ok


bind : Monad.Bind a (TResult i w e a) (TResult i w e b)
bind (CResult ka) callback =
  CResult <| \i w ->
    case ka i w of
      Rbad i1 w1 e ->
        Rbad i1 w1 e
      Rgood i1 w1 a ->
        case callback a of
          CResult kb -> kb i1 w1


andThen : Monad.AndThen a (TResult i w e a) (TResult i w e b)
andThen =
  Monad.andThen bind

-- PERF add INLINE to these?


foldM : Foldable.FoldlM a (TList a) b (TResult i w e b)
foldM =
  MList.foldlM return bind


traverseList : Traversable.Traverse a (TList a) (TResult i w e b) (TResult i w e (TList b))
traverseList =
  MList.traverse pure liftA2


sequenceAMap : Traversable.SequenceA (Map.Map comparable (TResult i w e a)) (TResult i w e (Map.Map comparable a))
sequenceAMap =
  Map.sequenceA pure liftA2


traverseMap : Traversable.Traverse a (Map.Map comparable a) (TResult i w e b) (TResult i w e (Map.Map comparable b))
traverseMap =
  Map.traverse pure liftA2


traverseWithKey : (comparable -> a -> TResult i w e b) -> Map.Map comparable a -> TResult i w e (Map.Map comparable b)
traverseWithKey =
  Map.traverseWithKey pure liftA2
