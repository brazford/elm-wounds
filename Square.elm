module Square exposing (..)
import Man

type SquareType = Normal | OffLimits | RedExit | BlueExit

type alias Square =
  { squareType : SquareType
    , occupant : Maybe Man.Man
  }
