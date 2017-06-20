module Board exposing (..)
import Array exposing (..)
import Man exposing (Man)
import Square exposing (..)
import Move exposing (..)
import Ability exposing (..)
import Player exposing (..)


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
  -- let
  --   test = Debug.log "index:" (rank * board.width) + file
  -- in
    (rank * board.width) + file


squareFileAndRankFromIndex : Board -> Int -> ( Int, Int )
squareFileAndRankFromIndex board index =
  ((rem index board.width), (index // board.width))


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
{--
moveMan : Int -> Int -> Board -> Board
moveMan fromIndex toIndex board =
  let
    fromSquare = Array.get fromIndex board.squares
    emptySquare = { occupant = Nothing, squareType = fromSquare.squareType }
    toSquare = Array.get toIndex board.squares
    occupiedSquare = { occupant = fromSquare.occupant, squareType = toSquare.squareType }
  in
    { board | squares = (Array.set fromIndex emptySquare board.squares) |> (Array.set toIndex occupiedSquare) }
--}

putMan : Man -> Int -> Int -> Board -> Board
putMan man file rank board =
  let
    theSquare = getSquare board file rank
    occupiedSquare = { occupant = Just man, squareType = theSquare.squareType }
  in
    { board | squares = Array.set (squareIndex board file rank) occupiedSquare board.squares }

clearMan : Int -> Int -> Board -> Board
clearMan file rank board =
  let
    theSquare = getSquare board file rank
    clearedSquare = { occupant = theSquare.occupant, squareType = theSquare.squareType }
  in
    { board | squares = Array.set (squareIndex board file rank) clearedSquare board.squares }

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

clearManFromIndex : Int -> Board -> Board
clearManFromIndex index board =
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


addMoveToList : Ability -> Board -> Int -> Int -> Man -> List Move -> List Move
addMoveToList ability board fromIndex toIndex man moveList =
  let
    file = (rem fromIndex board.width)
    rank = (fromIndex // board.width)
    toFile = (rem toIndex board.width) + ability.xOffset
    toRank = (toIndex // board.width) + ability.yOffset
    defendingMan = getMan board toFile toRank
    defendingAbility = Man.getDefendingAbility defendingMan ability
    nextIndex = (toRank * board.width) + toFile
    isLegalMove board toFile toRank man defendingMan =
      (toFile >= 0) && (toFile < board.width) && (toRank >= 0) && (toRank < board.height) && not (sameTeam defendingMan man)
  in
    if isLegalMove board toFile toRank man defendingMan then
      if (ability.abilityType == Slide) && (defendingMan == Nothing) then
        moveList ++ [Move file rank toFile toRank man defendingMan ability defendingAbility 0 0 0] ++ addMoveToList ability board fromIndex nextIndex man moveList
      else
        if defendingMan == Nothing then
          moveList ++ [Move file rank toFile toRank man defendingMan ability defendingAbility 0 0 0]
        else
          moveList ++ [Move file rank toFile toRank man defendingMan ability defendingAbility 0 0 100]
    else
      moveList


generateLegalMovesForPiece : Board -> Maybe Man -> Int -> Player -> List Move
generateLegalMovesForPiece board maybeMan index player =
  case maybeMan of
    Just maybeMan ->
      if maybeMan.player == player then
        let
          man = maybeMan
          moveList = []
          toIndex = index -- prime the pump for slide type moves
          moveListList = List.map (\ability -> addMoveToList ability board index toIndex man moveList) man.abilities
        in
          List.concat moveListList
      else
        []
    _ ->
      []

getAIMoves : Board -> Player -> List Move
getAIMoves board player =
  let
    squaresRange = List.range 0 ((Array.length board.squares) - 1)
    indexedList = Array.toIndexedList board.squares
    indexedOccupiedList = List.filter (\(i, s) -> s.occupant /= Nothing) indexedList
    moveListList = List.map (\(i, s) -> generateLegalMovesForPiece board s.occupant i player) indexedOccupiedList
  in
    List.concat moveListList

scoreMoveMobility : Board -> Player -> Move -> Move
scoreMoveMobility board player move =
  let
    hypothetical = makeMove board move
    moveList = getAIMoves hypothetical player
    mobility = List.length moveList
  in
    {move | mobility = mobility}

scoreBoard : Board -> Player -> Int
scoreBoard board player =
  let
    getScore man =
      case man of
        Just man ->
          Man.calculateValue man
        _ ->
          0
  in
    Array.toList board.squares |> List.map(\square -> getScore square.occupant) |> List.sum


scoreMoveMaterial : Board -> Player -> Move -> Move
scoreMoveMaterial board player move =
  let
    fromIndex = squareIndex board move.fromFile move.fromRank
    toIndex = squareIndex board move.toFile move.toRank
    occupant = getManFromIndex board toIndex
  in
    case occupant of
      Just occupant ->
        let
          defendingAbility = move.defendingAbility
          manValue = Man.calculateValue occupant
        in
          case move.defendingAbility of
            Just defendingAbility ->
              case (Ability.getDefenseResult move.attackingAbility defendingAbility) of
                Ability.AbilityDemoted ->
                  {move | score = move.mobility + 25, material = 25}
                Ability.AbilityRemoved ->
                  {move | score = move.mobility + 50, material = 50}
                Ability.PieceCaptured ->
                  {move | score = move.mobility + manValue, material = manValue}
            _ -> -- Ability.PieceCaptured
              {move | score = move.mobility + manValue, material = manValue}

      _ ->
        {move | score = move.mobility}


makeMove : Board -> Move -> Board
makeMove board move =
  let
    fromIndex = squareIndex board move.fromFile move.fromRank
    toIndex = squareIndex board move.toFile move.toRank
    test = Debug.log "fromIndex:" fromIndex
    test2 = Debug.log "toIndex:" toIndex

    occupant = getManFromIndex board toIndex
    defended =
      case occupant of
        Just occupant ->
          Man.hasDefendingAbility occupant move.attackingAbility
        _ ->
          False
    getDefenseResult move =
      let
        defendingAbility = move.defendingAbility
      in
        case move.defendingAbility of
          Just defendingAbility ->
            Ability.getDefenseResult move.attackingAbility defendingAbility
          _ ->
            Ability.PieceCaptured
    demoteAbility man =
      let
        defendingAbility = move.defendingAbility
      in
        case defendingAbility of
          Just defendingAbility ->
            Man.demoteAbility man defendingAbility
          _ ->
            man
    removeAbility man =
      let
        defendingAbility = move.defendingAbility
      in
        case defendingAbility of
          Just defendingAbility ->
            Man.removeAbility man defendingAbility
          _ ->
            man
  in
    case occupant of
      Just occupant ->
        --if defended then
          case (getDefenseResult move) of
            Ability.AbilityDemoted ->
              Debug.log "Ability.AbilityDemoted"
              putManAtIndex (demoteAbility occupant) toIndex board
            Ability.AbilityRemoved ->
              Debug.log "Ability.AbilityRemoved"
              putManAtIndex (removeAbility occupant) toIndex board
            Ability.PieceCaptured ->
              Debug.log "Ability.PieceCaptured"
              putManAtIndex move.attackingMan toIndex board |> clearManFromIndex fromIndex
        --else -- no opposing prong
          --Debug.log "no opposing prong"
          --putManAtIndex move.attackingMan toIndex board |> clearMan move.fromFile move.fromRank
      _ -> -- no piece in target square
        Debug.log "no piece in target square"
        --moveMan fromIndex toIndex board
        putManAtIndex move.attackingMan toIndex board |> clearManFromIndex fromIndex


debugMove : Move -> Move
debugMove move =
  let
    _ = Debug.log "fromFile " move.fromFile
    _ = Debug.log "fromRank " move.fromRank
    _ = Debug.log "toFile " move.toFile
    _ = Debug.log "toRank " move.toRank
    _ = Debug.log "mobility " move.mobility
    _ = Debug.log "material " move.material
    _ = Debug.log "score " move.score
  in
    move

makeAIMove : Player -> Board -> Board
makeAIMove player board =
  let
    moveList = getAIMoves board player
    scoredMobility = List.map (\move -> scoreMoveMobility board player move) moveList
    scoredMoves = List.map (\move -> scoreMoveMaterial board player move) scoredMobility
    sortedMoves = List.sortBy .score scoredMoves |> List.reverse --scoredMoves |> List.reverse
    sortedMoves2 = List.map (\move -> debugMove move) sortedMoves
    maybeMove = List.head sortedMoves
  in
    case maybeMove of
      Just maybeMove ->
        let _ = Debug.log "score " maybeMove.score
        in
          makeMove board maybeMove
      _ ->
        board
