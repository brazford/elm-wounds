module Ability exposing (..)
import Color exposing(Color)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)

type AbilityType = Step | Slide | Jump | Fat
type DefenseResult = AbilityRemoved | AbilityDemoted | PieceCaptured
type alias Ability =
  { abilityType : AbilityType
    , xOffset : Int
    , yOffset : Int
    , demoted : Bool
  }


opposite : Ability -> Ability
opposite ability =
  { ability | xOffset = -ability.xOffset, yOffset = -ability.yOffset }


getDefenseResult : Ability -> Ability -> DefenseResult
getDefenseResult attacker defender =
  let
    attackStrength = if attacker.abilityType == Slide && not attacker.demoted then 2 else 1
    defenseStrength = if defender.abilityType == Slide && not defender.demoted then 2 else 1
  in
    case attackStrength - defenseStrength of
      -1 ->
        AbilityDemoted
      0 ->
        AbilityRemoved
      _ ->
        PieceCaptured


stepNorth = { abilityType = Step, xOffset = 0, yOffset = -1, demoted = False }
stepEast = { abilityType = Step, xOffset = 1, yOffset = 0, demoted = False }
stepSouth = { abilityType = Step, xOffset = 0, yOffset = 1, demoted = False }
stepWest = { abilityType = Step, xOffset = -1, yOffset = 0, demoted = False }
stepNorthEast = { abilityType = Step, xOffset = 1, yOffset = -1, demoted = False }
stepSouthEast = { abilityType = Step, xOffset = 1, yOffset = 1, demoted = False }
stepSouthWest = { abilityType = Step, xOffset = -1, yOffset = 1, demoted = False }
stepNorthWest = { abilityType = Step, xOffset = -1, yOffset = -1, demoted = False }


fatNorth = { abilityType = Fat, xOffset = 0, yOffset = -1, demoted = False }
fatEast = { abilityType = Fat, xOffset = 1, yOffset = 0, demoted = False }
fatSouth = { abilityType = Fat, xOffset = 0, yOffset = 1, demoted = False }
fatWest = { abilityType = Fat, xOffset = -1, yOffset = 0, demoted = False }
fatNorthEast = { abilityType = Fat, xOffset = 1, yOffset = -1, demoted = False }
fatSouthEast = { abilityType = Fat, xOffset = 1, yOffset = 1, demoted = False }
fatSouthWest = { abilityType = Fat, xOffset = -1, yOffset = 1, demoted = False }
fatNorthWest = { abilityType = Fat, xOffset = -1, yOffset = -1, demoted = False }


slideNorth = { abilityType = Slide, xOffset = 0, yOffset = -1, demoted = False }
slideEast = { abilityType = Slide, xOffset = 1, yOffset = 0, demoted = False }
slideSouth = { abilityType = Slide, xOffset = 0, yOffset = 1, demoted = False }
slideWest = { abilityType = Slide, xOffset = -1, yOffset = 0, demoted = False }
slideNorthEast = { abilityType = Slide, xOffset = 1, yOffset = -1, demoted = False }
slideSouthEast = { abilityType = Slide, xOffset = 1, yOffset = 1, demoted = False }
slideSouthWest = { abilityType = Slide, xOffset = -1, yOffset = 1, demoted = False }
slideNorthWest = { abilityType = Slide, xOffset = -1, yOffset = -1, demoted = False }


jumpNorthNorthEast = { abilityType = Jump, xOffset = 1, yOffset = -2, demoted = False }
jumpEastNorthEast = { abilityType = Jump, xOffset = 2, yOffset = -1, demoted = False }
jumpEastSouthEast = { abilityType = Jump, xOffset = 2, yOffset = 1, demoted = False }
jumpSouthSouthEast = { abilityType = Jump, xOffset = 1, yOffset = 2, demoted = False }
jumpSouthSouthWest = { abilityType = Jump, xOffset = -1, yOffset = 2, demoted = False }
jumpWestSouthWest = { abilityType = Jump, xOffset = -2, yOffset = 1, demoted = False }
jumpWestNorthWest = { abilityType = Jump, xOffset = -2, yOffset = -1, demoted = False }
jumpNorthNorthWest = { abilityType = Jump, xOffset = -1, yOffset = -2, demoted = False }
