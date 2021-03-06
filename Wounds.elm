module Wounds exposing (..)

import Html
import Array exposing (..)
import Json.Decode as Json
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Task
import VirtualDom
import Window
import Debug

import Board exposing (..)
import Man exposing (..)
import ManView exposing (..)
import Player exposing(..)
import Move exposing(..)
import Ability
import Game


type alias Position =
    { x : Int, y : Int }


type alias FileRank =
    { x : Int, y : Int }


type Msg
    = Error
    | WindowSize Window.Size
    | BoardMouseMove Position
    | BoardMouseDown Position
    | BoardMouseUp Position


main : Program Never Model Msg
main =
  Html.program { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions model =
    Window.resizes WindowSize


type alias Model =
    { squareSize : Int
    , size : Window.Size
    , mousePosition : Position
    , pieceInHandPosition : FileRank
    , pieceInHand : Maybe Man
    , fromIndex : Int
    , board : Board
    , legalMoves : List Move
    , whoseTurn : Player
    }


init : ( Model, Cmd Msg )
init =
  let
    w = 10
    h = 8
    squareSize = 40
  in
    ( { squareSize = squareSize
      , size = Window.Size (w * squareSize) (h * squareSize)
      , mousePosition = Position 0 0
      , pieceInHandPosition = Position 0 0
      , pieceInHand = Nothing
      , fromIndex = 0
      , board = Game.setUpPowerChess
      , legalMoves = []
      , whoseTurn = Player.redPlayer
      }
    , Task.perform WindowSize Window.size
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of -- |> Debug.log "msg"
        WindowSize { width, height } ->
            ( { model | size = Window.Size ((width // 10) * 4) ((width // 10) * 4)
              , squareSize = (model.size.width // model.board.width)
              }
            , Cmd.none
            )

        BoardMouseMove pos ->
          let
            --one = Debug.log "MouseMove " pos
            pieceInHand = model.pieceInHand
          in
            case pieceInHand of
                Just pieceInHand ->
                    ( { model | pieceInHandPosition = pos }, Cmd.none )
                _ ->
                  ( model, Cmd.none )

        BoardMouseDown pos ->
          let
            one = Debug.log "BoardMouseDown: model.pieceInHandPosition " model.pieceInHandPosition
            pieceInHand = model.pieceInHand
            index = Board.squareIndexFromMousePosition model.board model.squareSize pos.x pos.y
            occupant = Board.getManFromIndex model.board index
          in
            if occupant /= Nothing then
              let
                legalMoves = Board.generateLegalMovesForPiece model.board occupant index model.whoseTurn
              in
                if List.length legalMoves > 0 then
                  ( { model | pieceInHand = occupant
                    , pieceInHandPosition = pos
                    , fromIndex = index
                    , board = Board.clearManFromIndex index model.board
                    , legalMoves = legalMoves
                    }
                  , Cmd.none )
                else
                  ( model, Cmd.none )
            else
              ( model, Cmd.none )

        BoardMouseUp pos ->
          let
            pieceInHand = model.pieceInHand
            index = Board.squareIndexFromMousePosition model.board model.squareSize pos.x pos.y
            occupant = Board.getManFromIndex model.board index
            legalDestination move index =
              ( move.toFile, move.toRank ) == (squareFileAndRankFromIndex model.board index)
            possibleMoves = List.filter (\move -> legalDestination move index) model.legalMoves
            thisMaybeMove = List.head possibleMoves

            defended defender =
              case thisMaybeMove of
                Just thisMaybeMove ->
                  Man.hasDefendingAbility defender thisMaybeMove.attackingAbility
                _ ->
                  False

            getDefenseResult thisMaybeMove =
              case thisMaybeMove of
                Just thisMaybeMove ->
                  let
                    defendingAbility = thisMaybeMove.defendingAbility
                  in
                    case thisMaybeMove.defendingAbility of
                      Just defendingAbility ->
                        Ability.getDefenseResult thisMaybeMove.attackingAbility defendingAbility
                      _ ->
                        Ability.PieceCaptured
                _ ->
                  Ability.PieceCaptured

            demoteAbility man =
              case thisMaybeMove of
                Just thisMaybeMove ->
                  let
                    defendingAbility = thisMaybeMove.defendingAbility
                  in
                    case defendingAbility of
                      Just defendingAbility ->
                        Man.demoteAbility man defendingAbility
                      _ ->
                        man
                _ ->
                  man

            removeAbility man =
              case thisMaybeMove of
                Just thisMaybeMove ->
                  let
                    defendingAbility = thisMaybeMove.defendingAbility
                  in
                    case defendingAbility of
                      Just defendingAbility ->
                        Man.removeAbility man defendingAbility
                      _ ->
                        man
                _ ->
                  man

          in
            case pieceInHand of
                Just pieceInHand ->
                  if List.length possibleMoves > 0 then
                    case occupant of
                      Just occupant ->
                        if defended occupant then
                          case (getDefenseResult thisMaybeMove) |> Debug.log "getDefenseResult" of
                            Ability.AbilityDemoted ->
                              ( { model | pieceInHand = Nothing
                                , board = Board.putManAtIndex pieceInHand model.fromIndex model.board
                                        |> Board.putManAtIndex (demoteAbility occupant) index
                                        |> Board.makeAIMove (Player.getNextPlayer model.whoseTurn)
                                , legalMoves = []
                                }, Cmd.none )

                            Ability.AbilityRemoved ->
                              ( { model | pieceInHand = Nothing
                                , board = Board.putManAtIndex pieceInHand model.fromIndex model.board
                                        |> Board.putManAtIndex (removeAbility occupant) index
                                        |> Board.makeAIMove (Player.getNextPlayer model.whoseTurn)
                                , legalMoves = []
                                }, Cmd.none )

                            _ -> -- Ability.PieceCaptured
                            ( { model | pieceInHand = Nothing
                              , board = Board.putManAtIndex pieceInHand index model.board
                                        |> Board.makeAIMove (Player.getNextPlayer model.whoseTurn)
                              , legalMoves = []
                              }, Cmd.none )
                        else -- no opposing prong
                          ( { model | pieceInHand = Nothing
                            , board = Board.putManAtIndex pieceInHand index model.board
                                        |> Board.makeAIMove (Player.getNextPlayer model.whoseTurn)
                            , legalMoves = []
                            }, Cmd.none )

                      _ -> -- no piece in target square
                        ( { model | pieceInHand = Nothing
                          , board = Board.putManAtIndex pieceInHand index model.board
                                      |> Board.makeAIMove (Player.getNextPlayer model.whoseTurn)
                          , legalMoves = []
                          }, Cmd.none )
                  else -- no legal moves for this piece
                    ( { model | pieceInHand = Nothing
                      , board = Board.putManAtIndex pieceInHand model.fromIndex model.board
                      , legalMoves = []
                      }, Cmd.none )
                _ -> -- no piece clicked on, we probably never get here
                  ( model, Cmd.none )

        _ ->
            Debug.crash "update"


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ scene model
{--        , Html.div
            []
            [ Html.text (toString model.legalMoves) ]--}
        ]


scene : Model -> Html.Html Msg
scene model =
    Svg.svg
        [ width <| toString ( model.board.width * model.squareSize )
        , height <| toString ( model.board.height * model.squareSize )
        ]
        [ chessBoardView model
        , legalMovesView model
        , piecesView model
        , clickCatcher model
        ]


px : a -> String
px n =
    toString n ++ "px"


chessBoardView : Model -> Svg.Svg Msg
chessBoardView model =
  let
    getOddness index = (rem ((rem index model.board.height) + (index // model.board.width)) 2) == 1
    squaresRange = List.range 0 ((Array.length model.board.squares) - 1)
    getColor isOdd =
      if isOdd then
        "#a0caa0"
      else
        "#cec"

    drawSquare index oddness =
      Svg.rect
          [ x <| toString <| ((rem index model.board.width) * model.squareSize)
          , y <| toString <| ((index // model.board.width) * model.squareSize)
          , width <| toString <| model.squareSize
          , height <| toString <| model.squareSize
          , fill (getColor oddness)
          , style "stroke:rgb(110,160,110);stroke-width:2"
          ]
          []

  in
    Svg.g
      []
      (List.map (\index -> drawSquare index (getOddness index)) squaresRange)

legalMovesView : Model -> Svg.Svg Msg
legalMovesView model =
  let
    hiliteSquare move =
      Svg.rect
        [ x <| toString <| (move.toFile * model.squareSize)
        , y <| toString <| (move.toRank * model.squareSize)
        , width <| toString <| model.squareSize
        , height <| toString <| model.squareSize
        , style "stroke:rgb(255,0,0);stroke-width:2;fill-opacity:0.0"
        ]
        []
  in
    Svg.g
      []
      (List.map (\move -> hiliteSquare move) model.legalMoves)



addManViewToList : Maybe Man -> Board -> Int -> Int -> List (Svg.Svg Msg) -> List (Svg.Svg Msg)
addManViewToList man board index squareSize manViewList =
  let
    centerOffset = squareSize // 2
    x = ((rem index board.width) * squareSize) + centerOffset
    y = ((index // board.width) * squareSize) + centerOffset
  in
    case man of
      Just man ->
        manViewList ++ [ManView.drawView man x y squareSize ]
      Nothing ->
        manViewList


addPieceInHandToList : Maybe Man -> Board -> Int -> Int -> Int -> List (Svg.Svg Msg) -> List (Svg.Svg Msg)
addPieceInHandToList man board x y squareSize manViewList =
  let
    centerOffset = squareSize // 2
  in
    case man of
      Just man ->
        manViewList ++ [ManView.drawView man x y squareSize ]
      Nothing ->
        manViewList


piecesView : Model -> Svg.Svg Msg
piecesView model =
  let
    squaresRange = List.range 0 ((Array.length model.board.squares) - 1)
    indexedList = Array.toIndexedList model.board.squares
    indexedOccupiedList = List.filter (\(i, s) -> s.occupant /= Nothing) indexedList
    indexedMenList = List.map (\ (i, s) -> (i, s.occupant)) indexedOccupiedList
    drawManFunc = (\(i, man) -> addManViewToList man model.board i model.squareSize [])
    manViewListList = List.map drawManFunc indexedMenList
    manViewList = List.concat manViewListList
    x = model.pieceInHandPosition.x
    y = model.pieceInHandPosition.y
    manViewListWithPieceInHand = addPieceInHandToList model.pieceInHand model.board x y model.squareSize manViewList
  in
    Svg.g
      []
      manViewListWithPieceInHand


{-| These options are an attempt to prevent double- and triple-clicking from
propagating and selecting text outside the SVG scene. Doesn't work.
-}
options =
    { preventDefault = True, stopPropagation = True }


offsetPosition : Json.Decoder Position
offsetPosition =
    Json.map2 Position (Json.field "offsetX" Json.int) (Json.field "offsetY" Json.int)


clickCatcher : Model -> Svg Msg
clickCatcher model =
  Svg.rect
    [ x "0"
    , y "0"
    , width (toString (model.squareSize * model.board.width))
    , height (toString (model.squareSize * model.board.height))
    , style "fill:purple;fill-opacity:0.0"
    , VirtualDom.onWithOptions "mousedown" options (Json.map BoardMouseDown offsetPosition)
    , VirtualDom.onWithOptions "mouseup" options (Json.map BoardMouseUp offsetPosition)
    , VirtualDom.onWithOptions "mousemove" options (Json.map BoardMouseMove offsetPosition)
    ]
    []

{--
circle : Model -> Svg Msg
circle model =
  Svg.circle
    [ cx (toString ( ( model.circlePosition.x * model.squareSize ) + (model.squareSize // 2) ) )
    , cy (toString ( ( model.circlePosition.y * model.squareSize ) + (model.squareSize // 2) ) )
    , r (toString ( model.squareSize // 2 ) )
    , style "stroke:rgb(255,0,0);stroke-width:2"
    , VirtualDom.onWithOptions "click" options (Json.map CircleClick offsetPosition)
    ]
    []


square : Model -> Svg Msg
square model =
  Svg.rect
    [ x (toString ( model.squarePosition.x * model.squareSize ) )
    , y (toString ( model.squarePosition.y * model.squareSize ) )
    , width (toString model.squareSize)
    , height (toString model.squareSize)
    , style "stroke:rgb(0,255,0);stroke-width:2"
    , VirtualDom.onWithOptions "click" options (Json.map SquareClick offsetPosition)
    ]
    []
--}
