module Man exposing (..)
import Player exposing (..)
import Ability exposing (..)


type alias Man =
  { abilities : List Ability
    , name : String
    , player : Player
  }


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
