module Man exposing (..)
import Player exposing (..)
import Ability exposing (..)
import Debug

type alias Man =
  { abilities : List Ability
    , name : String
    , player : Player
  }


hasDefendingAbility : Man -> Ability -> Bool
hasDefendingAbility defender attackingAbility =
  let
    abilityMatcher defendingAbility =
      (defendingAbility.xOffset == -attackingAbility.xOffset) && (defendingAbility.yOffset == -attackingAbility.yOffset)
    matches = List.filter (\ability -> abilityMatcher ability) defender.abilities
  in
    List.length matches > 0


getDefendingAbility : Maybe Man -> Ability -> Maybe Ability
getDefendingAbility defender attackingAbility =
  let
    abilityMatcher defendingAbility =
      (defendingAbility.xOffset == -attackingAbility.xOffset) && (defendingAbility.yOffset == -attackingAbility.yOffset)
  in
    case defender of
      Just defender ->
        List.head (List.filter (\ability -> abilityMatcher ability) defender.abilities)
      _ ->
        Nothing


demoteAbility : Man -> Ability -> Man
demoteAbility man abilityToDemote =
  let
    demoteMatchingAbilities candidateAbility =
      if (candidateAbility.xOffset ==  abilityToDemote.xOffset) && (candidateAbility.yOffset ==  abilityToDemote.yOffset) then
        { candidateAbility | demoted = True }
      else
        candidateAbility
  in
    { man | abilities = List.map (\candidateAbility -> demoteMatchingAbilities candidateAbility) man.abilities }


removeAbility : Man -> Ability -> Man
removeAbility man abilityToRemove =
  let
    nonMatchingAbilities candidate =
      abilityToRemove.xOffset /= candidate.xOffset || abilityToRemove.yOffset /= candidate.yOffset
      {--
      if abilityToRemove.xOffset /= candidate.xOffset then
        True
      else
        abilityToRemove.yOffset /= candidate.yOffset--}
  in
    { man | abilities = List.filter (\ability -> nonMatchingAbilities ability) man.abilities }


star : Player -> Man
star player =
  { player = player
    , name = "star"
    , abilities =
      [ Ability.stepNorth
        , Ability.stepNorthEast
        , Ability.stepEast
        , Ability.stepSouthEast
        , Ability.stepSouth
        , Ability.stepSouthWest
        , Ability.stepWest
        , Ability.stepNorthWest
      ]
  }


king : Player -> Man
king player =
  { player = player
    , name = "king"
    , abilities =
      [ Ability.fatNorth
        , Ability.fatNorthEast
        , Ability.fatEast
        , Ability.fatSouthEast
        , Ability.fatSouth
        , Ability.fatSouthWest
        , Ability.fatWest
        , Ability.fatNorthWest
      ]
  }


queen : Player -> Man
queen player =
  { player = player
    , name = "queen"
    , abilities =
      [ Ability.slideNorth
        , Ability.slideNorthEast
        , Ability.slideEast
        , Ability.slideSouthEast
        , Ability.slideSouth
        , Ability.slideSouthWest
        , Ability.slideWest
        , Ability.slideNorthWest
      ]
  }


rook : Player -> Man
rook player =
  { player = player
    , name = "rook"
    , abilities =
      [ Ability.slideNorth
        , Ability.slideEast
        , Ability.slideSouth
        , Ability.slideWest
      ]
  }


bishop : Player -> Man
bishop player =
  { player = player
    , name = "bishop"
    , abilities =
      [ Ability.slideNorthEast
        , Ability.slideSouthEast
        , Ability.slideSouthWest
        , Ability.slideNorthWest
      ]
  }


knight : Player -> Man
knight player =
  { player = player
    , name = "knight"
    , abilities =
      [ Ability.jumpNorthNorthEast
        , Ability.jumpEastNorthEast
        , Ability.jumpEastSouthEast
        , Ability.jumpSouthSouthEast
        , Ability.jumpSouthSouthWest
        , Ability.jumpWestSouthWest
        , Ability.jumpWestNorthWest
        , Ability.jumpNorthNorthWest
      ]
  }


knightBishop : Player -> Man
knightBishop player =
  { player = player
    , name = "knightBishop"
    , abilities =
      [ Ability.slideNorthEast
        , Ability.slideSouthEast
        , Ability.slideSouthWest
        , Ability.slideNorthWest
        , Ability.jumpNorthNorthEast
        , Ability.jumpEastNorthEast
        , Ability.jumpEastSouthEast
        , Ability.jumpSouthSouthEast
        , Ability.jumpSouthSouthWest
        , Ability.jumpWestSouthWest
        , Ability.jumpWestNorthWest
        , Ability.jumpNorthNorthWest
      ]
  }


knightRook : Player -> Man
knightRook player =
  { player = player
    , name = "knightRook"
    , abilities =
      [ Ability.slideNorth
        , Ability.slideEast
        , Ability.slideSouth
        , Ability.slideWest
        , Ability.jumpNorthNorthEast
        , Ability.jumpEastNorthEast
        , Ability.jumpEastSouthEast
        , Ability.jumpSouthSouthEast
        , Ability.jumpSouthSouthWest
        , Ability.jumpWestSouthWest
        , Ability.jumpWestNorthWest
        , Ability.jumpNorthNorthWest
      ]
  }
