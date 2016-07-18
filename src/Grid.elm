
module Grid exposing (..)


-- Core
import Array exposing (Array)
-- 3rd
import Html
import Html.Attributes
import Svg
import Svg.Attributes as Attr
import Svg.Events as Events
import Mouse


type alias Grid =
  Array (Array (Maybe (Int, Int)))


empty : Int -> Int -> Grid
empty rows cols =
  let
    makeTile y x =
      Nothing
    makeRow y =
      Array.map (makeTile y) (Array.fromList [0 .. cols])
  in
    Array.map makeRow (Array.fromList [0 .. rows])


viewTile : Context msg -> Int -> Int -> Maybe (Int, Int) -> Svg.Svg msg
viewTile ctx y x maybeCoord =
  Svg.rect
  [ Attr.width "48"
  , Attr.height "48"
  , Attr.x (toString (x * 48))
  , Attr.y (toString (y * 48))
  , Attr.stroke "black"
  , Attr.fill "transparent"
  , Events.onClick (ctx.onTileClick x y)
  ]
  []



viewRow : Context msg -> Int -> Array (Maybe (Int, Int)) -> Svg.Svg msg
viewRow ctx y row =
  Svg.g [] (List.indexedMap (viewTile ctx y) (Array.toList row))


view : Context msg -> Grid -> Html.Html msg
view ctx grid =
  let
    translate =
      "translate("
      ++ toString ctx.offset.x ++ " "
      ++ toString ctx.offset.y
      ++ ") "
    transform =
      Attr.transform translate
  in
    Svg.svg
    [ Attr.width "600"
    , Attr.height "400"
    , Attr.viewBox "0 0 200 200"
    , ctx.onMouseDown
    ]
    [ Svg.g
      [ transform ]
      (List.indexedMap (viewRow ctx) (Array.toList grid))

    ]


type alias Context msg =
  { onTileClick : (Int -> Int -> msg)
  , onMouseDown : Html.Attribute msg
  , offset : Mouse.Position
  }
