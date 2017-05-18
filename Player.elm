module Player exposing (..)
type alias Player =
  { name : String
    , directionX : Int
    , directionY : Int
  }


redPlayer =
  { name = "Red"
    , directionX = 0
    , directionY = -1
  }


bluePlayer =
  { name = "Blue"
    , directionX = 0
    , directionY = 1
  }
