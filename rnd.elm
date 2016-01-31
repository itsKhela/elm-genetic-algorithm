module Rnd where

import Random
import Graphics.Element exposing (..)
import Native.Randoms

main = show ((lista 3 1000 10020) ++ (lista 2 1001 10022))

seed0 = round (Native.Randoms.getFloat)

ranm seed ci cs =
  let
    rnd = Random.generate (Random.int ci cs) (Random.initialSeed seed)
    result =
    case rnd of
      (res,_) -> res
  in
    result

lista n ci cs = List.foldl (\x acc -> (ranm x ci cs) :: acc) [] [(1+seed0)..(n+seed0)]
