module Game exposing (..)
import Board exposing (..)
import Array exposing (..)
import Square exposing (..)
import Man exposing (..)
import Player exposing (..)

type alias Level =
  { name : String
    , board : Board
  }
type alias Game =
  { name : String
    , levels : Array Level
    , board : Board
  }

setUpPowerChess : Board
setUpPowerChess =
  let
    w = 10
    h = 8
    powerChessBoard =
      { width = w
        , height = h
        , squares = Array.repeat (w * h) { squareType = Normal, occupant = Nothing }
      }
  in
    powerChessBoard
    |> Board.putMan (Man.rook Player.bluePlayer) 0 0
    |> Board.putMan (Man.knight Player.bluePlayer) 1 0
    |> Board.putMan (Man.bishop Player.bluePlayer) 2 0
    |> Board.putMan (Man.knightBishop Player.bluePlayer) 3 0
    |> Board.putMan (Man.king Player.bluePlayer) 4 0
    |> Board.putMan (Man.queen Player.bluePlayer) 5 0
    |> Board.putMan (Man.knightRook Player.bluePlayer) 6 0
    |> Board.putMan (Man.bishop Player.bluePlayer) 7 0
    |> Board.putMan (Man.knight Player.bluePlayer) 8 0
    |> Board.putMan (Man.rook Player.bluePlayer) 9 0
    |> Board.putMan (Man.star Player.bluePlayer) 0 1
    |> Board.putMan (Man.star Player.bluePlayer) 1 1
    |> Board.putMan (Man.star Player.bluePlayer) 2 1
    |> Board.putMan (Man.star Player.bluePlayer) 3 1
    |> Board.putMan (Man.star Player.bluePlayer) 4 1
    |> Board.putMan (Man.star Player.bluePlayer) 5 1
    |> Board.putMan (Man.star Player.bluePlayer) 6 1
    |> Board.putMan (Man.star Player.bluePlayer) 7 1
    |> Board.putMan (Man.star Player.bluePlayer) 8 1
    |> Board.putMan (Man.star Player.bluePlayer) 9 1
    |> Board.putMan (Man.rook Player.redPlayer) 0 7
    |> Board.putMan (Man.knight Player.redPlayer) 1 7
    |> Board.putMan (Man.bishop Player.redPlayer) 2 7
    |> Board.putMan (Man.knightBishop Player.redPlayer) 3 7
    |> Board.putMan (Man.king Player.redPlayer) 4 7
    |> Board.putMan (Man.queen Player.redPlayer) 5 7
    |> Board.putMan (Man.knightRook Player.redPlayer) 6 7
    |> Board.putMan (Man.bishop Player.redPlayer) 7 7
    |> Board.putMan (Man.knight Player.redPlayer) 8 7
    |> Board.putMan (Man.rook Player.redPlayer) 9 7
    |> Board.putMan (Man.star Player.redPlayer) 0 6
    |> Board.putMan (Man.star Player.redPlayer) 1 6
    |> Board.putMan (Man.star Player.redPlayer) 2 6
    |> Board.putMan (Man.star Player.redPlayer) 3 6
    |> Board.putMan (Man.star Player.redPlayer) 4 6
    |> Board.putMan (Man.star Player.redPlayer) 5 6
    |> Board.putMan (Man.star Player.redPlayer) 6 6
    |> Board.putMan (Man.star Player.redPlayer) 7 6
    |> Board.putMan (Man.star Player.redPlayer) 8 6
    |> Board.putMan (Man.star Player.redPlayer) 9 6
