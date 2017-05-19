module Board exposing (..)
import Array exposing (..)
import Man exposing (Man)
import Square exposing (..)
import Move exposing (..)
import Ability exposing (..)


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


sameTeam : Maybe Man -> Man -> Bool
sameTeam maybeMan man =
  case maybeMan of
    Just maybeMan ->
      maybeMan.player == man.player
    _ ->
      False

{--
addMoveToList : Ability -> Board -> Int -> Man -> List Move -> List Move
addMoveToList ability board index man moveList =
  let
    file = (rem index board.width)
    rank = (index // board.width)
    toFile = file + ability.xOffset
    toRank = rank + ability.yOffset
    defendingMan = getMan board toFile toRank
    legal =
      (toFile >= 0) && (toFile < board.width) && (toRank >= 0) && (toRank < board.height) && not (sameTeam defendingMan man)
  in
    if legal then
      moveList ++ [Move file rank toFile toRank man Nothing ability Nothing]
    else
      moveList
--}

addMoveToList : Ability -> Board -> Int -> Man -> List Move -> List Move
addMoveToList ability board index man moveList =
  let
    showRank = Debug.log "rank " rank
    file = (rem index board.width)
    rank = (index // board.width)
    toFile = file + ability.xOffset
    toRank = rank + ability.yOffset
    defendingMan = getMan board toFile toRank
    nextIndex = (toRank * board.width) + toFile
    isLegal board toFile toRank man defendingMan =
      (toFile >= 0) && (toFile < board.width) && (toRank >= 0) && (toRank < board.height) && not (sameTeam defendingMan man)
  in
    if isLegal board toFile toRank man defendingMan then
      if (ability.abilityType == Slide) && (defendingMan == Nothing) then
        moveList ++ [Move file rank toFile toRank man defendingMan ability Nothing] ++ addMoveToList ability board nextIndex man moveList
      else
        moveList ++ [Move file rank toFile toRank man defendingMan ability Nothing]
    else
      moveList


generateLegalMovesForPiece : Board -> Maybe Man -> Int -> List Move
generateLegalMovesForPiece board maybeMan index =
  case maybeMan of
    Just maybeMan ->
      let
        man = maybeMan
        moveList = []
        moveListList = List.map (\ability -> addMoveToList ability board index man moveList) man.abilities
      in
        List.concat moveListList
        --[ Move 0 6 0 5 man Nothing Ability.stepNorth Nothing]
    _ ->
      []
