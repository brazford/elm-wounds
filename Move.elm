module Move exposing (..)

import Ability exposing (..)
import Man exposing (..)


type alias Move =
  { fromFile : Int
  , fromRank : Int
  , toFile : Int
  , toRank : Int
  , attackingMan : Man
  , defendingMan : Maybe Man
  , attackingAbility : Ability
  , defendingAbility : Maybe Ability
  }
