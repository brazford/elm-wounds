module ManView exposing (..)
import Man exposing (..)
import AbilityView
import Svg
import Color exposing(rgb)
import Board exposing (..)
import Player exposing (..)


drawView : Man -> Int -> Int -> Int -> Svg.Svg msg
drawView man x y squareSize =
  let
    color =
      if man.player == bluePlayer then
        "blue"
      else
        "red"
  in
    Svg.g
    []
    (List.map (\ability -> AbilityView.drawAbility x y squareSize color ability) man.abilities)
