
module Grid exposing (..)


-- Core
import Array exposing (Array)
-- 3rd
import Html
import Html.Attributes as Hattr
import Svg
import Svg.Attributes as Attr
import Svg.Events as Events
import Mouse
import Array.Extra as AE


type alias Grid =
  Array (Array (Maybe (Int, Int)))


-- CREATE


empty : Int -> Int -> Grid
empty rows cols =
  let
    makeTile y x =
      Nothing
    makeRow y =
      Array.map (makeTile y) (Array.fromList [0 .. cols])
  in
    Array.map makeRow (Array.fromList [0 .. rows])


-- UPDATE TILES


setTile : Int -> Int -> Maybe (Int, Int) -> Grid -> Grid
setTile x y val grid =
  AE.update y (\row -> AE.update x (\tile -> val) row) grid


getTile : Int -> Int -> Grid -> Maybe (Int, Int)
getTile x y grid =
  Maybe.andThen (Array.get y grid)
  <| \row -> Maybe.withDefault Nothing (Array.get x row)


-- VIEW


viewTile : Context msg -> Int -> Int -> Maybe (Int, Int) -> Svg.Svg msg
viewTile ({tilesize} as ctx) y x maybeCoord =
  let
    (fillOpacity, fill) =
         case maybeCoord of
           Nothing ->
             ("0.5", "grey")
           Just coord ->
             ("1.0", "url(#tile-" ++ toString (fst coord) ++ "-" ++ toString (snd coord) ++ ")")
  in
    Svg.g
    []
    [ Svg.rect
      [ Attr.width (toString tilesize)
      , Attr.height (toString tilesize)
      , Attr.x (toString (x * tilesize))
      , Attr.y (toString (y * tilesize))
      , Attr.stroke "black"
      , Attr.fill fill
      , Attr.fillOpacity fillOpacity
      , Attr.class "tile"
      , Events.onMouseOut (ctx.onMouseOver x y)
      , Events.onMouseOver (ctx.onMouseOver x y)
      , Events.onClick (ctx.onTileClick x y)
      ]
      []
    ]


viewRow : Context msg -> Int -> Array (Maybe (Int, Int)) -> Svg.Svg msg
viewRow ctx y row =
  Svg.g [] (List.indexedMap (viewTile ctx y) (Array.toList row))


-- No energy left to fix the following clusters
generatePairs : Int -> Int -> List (Int, Int)
generatePairs rows cols =
  List.concat <| List.indexedMap (\i xs -> List.map (\x -> (x, i)) xs ) (List.repeat rows [0 .. cols-1])


viewPatterns : Context msg -> List (Html.Html msg)
viewPatterns ({tilesize, rows, cols} as ctx)  =
  let
    fourth = tilesize // 4
    viewBox x y =
      toString (x * fourth) ++ " "
      ++ toString ((y + 1) * fourth) ++ " "
      ++ " " ++ toString fourth ++ " " ++ toString fourth
    makeDef (x, y) =
      Svg.pattern
      [ Attr.id <| "tile-" ++ toString x ++ "-" ++ toString y
      , Attr.width <| toString tilesize -- "48"
      , Attr.height <| toString tilesize -- "48"
      , Attr.viewBox (viewBox x y)
      , Attr.patternUnits "userSpaceOnUse"
      ]
      [ Svg.image
        [ Attr.xlinkHref ctx.path
        , Attr.width <| toString tilesize --"48"
        , Attr.height <| toString tilesize --"48"
        ]
        []
      ]
  in
    List.map makeDef (generatePairs rows cols)


view : Context msg -> Grid -> Html.Html msg
view ctx grid =
  let
    translate =
      "translate("
      ++ toString ctx.offset.x ++ " "
      ++ toString ctx.offset.y
      ++ ") "
      ++ "scale(" ++ toString ctx.scale ++ ")"
    transform =
      Attr.transform translate
  in
    Svg.svg
    [ Attr.class "tile-map"
    , ctx.onMouseDown
    , Hattr.style
        [ ( "background-position",
            (toString <| ctx.offset.x // -2) ++ "px "
            ++ (toString <| ctx.offset.y // -2) ++ "px"
          )
        ]
    ]
    [ Svg.defs
      []
      (viewPatterns ctx)
    , Svg.g
      [ transform ]
      (List.indexedMap (viewRow ctx) (Array.toList grid))
    ]


type alias Context msg =
  { onTileClick : (Int -> Int -> msg)
  , onMouseDown : Html.Attribute msg
  , onMouseOver : (Int -> Int -> msg)
  , offset : Mouse.Position
  , path : String
  , tilesize : Int
  , rows : Int
  , cols : Int
  , scale : Float
  }
