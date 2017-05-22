module AbilityView exposing (..)
import Ability exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

drawStep : Int -> Int -> Int -> String -> Ability -> Svg.Svg msg
drawStep x y squareSize color ability =
  let
    thin = squareSize // 20
    shortLength = squareSize // 5
  in
    Svg.line
      [ stroke color
      , strokeWidth (toString thin)
      , x1 (toString x)
      , x2 (toString (x + (shortLength * ability.xOffset)))
      , y1 (toString y)
      , y2 (toString (y + (shortLength * ability.yOffset)))
      ] []

drawJump : Int -> Int -> Int -> String -> Ability -> Svg.Svg msg
drawJump x y squareSize color ability =
  let
    thin = squareSize // 40
    shortLength = squareSize // 5
  in
    Svg.line
      [ stroke color
      , strokeWidth (toString thin)
      , x1 (toString x)
      , x2 (toString (x + (shortLength * ability.xOffset)))
      , y1 (toString y)
      , y2 (toString (y + (shortLength * ability.yOffset)))
      , strokeDasharray "2, 2"
      ] []


drawFat : Int -> Int -> Int -> String -> Ability -> Svg.Svg msg
drawFat x y squareSize color ability =
  let
    getStrokeThickness =
      if ability.demoted then
        squareSize // 20
      else
        squareSize // 10

    shortLength = squareSize // 5
  in
    Svg.line
      [ stroke color
      , strokeWidth (toString getStrokeThickness)
      , x1 (toString x)
      , x2 (toString (x + (shortLength * ability.xOffset)))
      , y1 (toString y)
      , y2 (toString (y + (shortLength * ability.yOffset)))
      ] []



drawSlide : Int -> Int -> Int -> String -> Ability -> Svg.Svg msg
drawSlide x y squareSize color ability =
  let
    thin = squareSize // 20
    thick = squareSize // 10
    shortLength = squareSize // 5
    longLength = squareSize // 3
    drawConditionally =
      if ability.demoted then
        Svg.line
          [ stroke color
          , strokeWidth (toString thin)
          , x1 (toString x)
          , x2 (toString (x + (shortLength * ability.xOffset)))
          , y1 (toString y)
          , y2 (toString (y + (shortLength * ability.yOffset)))
          ] []
      else
        Svg.g
        []
        [ Svg.line
            [ stroke color
            , strokeWidth (toString thick)
            , x1 (toString x)
            , x2 (toString (x + (shortLength * ability.xOffset)))
            , y1 (toString y)
            , y2 (toString (y + (shortLength * ability.yOffset)))
            ] []


          , Svg.line
            [ stroke color
            , strokeWidth (toString thin)
            , x1 (toString x)
            , x2 (toString (x + (longLength * ability.xOffset)))
            , y1 (toString y)
            , y2 (toString (y + (longLength * ability.yOffset)))
            ] []
        ]

  in
    drawConditionally

drawAbility : Int -> Int -> Int -> String -> Ability -> Svg.Svg msg
drawAbility x y squareSize color ability =
  case ability.abilityType of
    Step ->
      drawStep x y squareSize color ability
    Fat ->
      drawFat x y squareSize color ability
    Jump ->
      drawJump x y squareSize color ability
    Slide ->
      drawSlide x y squareSize color ability
