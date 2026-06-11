{- MANUALLY FORMATTED -}
module Compiler.Reporting.Suggest exposing
  ( distance
  , sort
  , rank
  )


import Extra.Text.EditDistance as Dist
import Extra.Type.List as MList exposing (TList)



-- DISTANCE


distance : String -> String -> Int
distance x y =
  Dist.distance x y



-- SORT


sort : String -> (a -> String) -> TList a -> TList a
sort target toString values =
  MList.sortOn (distance (String.toLower target) << String.toLower << toString) values



-- RANK


rank : String -> (a -> String) -> TList a -> TList (Int,a)
rank target toString values =
  let
    toRank v =
      distance (String.toLower target) (String.toLower (toString v))

    addRank v =
      (toRank v, v)
  in
  MList.sortOn Tuple.first (MList.map addRank values)
