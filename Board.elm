module Board exposing (..)
import Array exposing (..)
import Man exposing (Man)
import Square exposing (..)


type alias Board =
  { width : Int
  , height : Int
  , squares : Array Square
  }


squareIndexFromMousePosition : Board -> Int -> Int -> Int -> Int
squareIndexFromMousePosition board squareSize x y =
  let
    file = x // squareSize
    rank = y // squareSize
  in
    squareIndex board file rank


squareIndex : Board -> Int -> Int -> Int
squareIndex board file rank =
  (rank * board.width) + file


getSquare : Board -> Int -> Int -> Square
getSquare board file rank =
  let
    gottenSquare = Array.get ( squareIndex board file rank ) board.squares
  in
    case gottenSquare of
      Just gottenSquare ->
        gottenSquare
      Nothing ->
        { squareType = Normal, occupant = Nothing}


putMan : Man -> Int -> Int -> Board -> Board
putMan man file rank board =
  let
    theSquare = getSquare board file rank
    occupiedSquare = { occupant = Just man, squareType = theSquare.squareType }
  in
    { board | squares = Array.set (squareIndex board file rank) occupiedSquare board.squares }


putManAtIndex : Man -> Int -> Board -> Board
putManAtIndex man index board =
  let
    file = (rem index board.width)
    rank = (index // board.width)
  in
    putMan man file rank board


getMan : Board -> Int -> Int -> Maybe Man
getMan board file rank =
  let
    theSquare = getSquare board file rank
  in
    theSquare.occupant

clearManFromIndex : Board -> Int -> Board
clearManFromIndex board index =
  { board | squares = Array.set index { squareType = Normal, occupant = Nothing} board.squares }


getManFromIndex : Board -> Int -> Maybe Man
getManFromIndex board index =
  let
    gottenSquare = Array.get ( index ) board.squares
  in
    case gottenSquare of
      Just gottenSquare ->
        gottenSquare.occupant
      Nothing ->
        Nothing
