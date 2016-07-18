
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
  let _ = Debug.log "setTime" (x, y) in
  AE.update y (\row -> AE.update x (\tile -> val) row) grid


-- VIEW


viewImage : Context msg -> (Int, Int) -> Int -> Int -> Svg.Svg msg
viewImage ctx (col, row) x y =
  -- Svg.defs
  -- []
  -- [ Svg.view
  --   [ Attr.width "48"
  --   , Attr.height "48"
  --   , Attr.patternUnits "userSpaceOnUse"
  --   ]
  --   [ Svg.image
  --     [ Attr.xlinkHref ctx.path
  --     , Attr.width "48"
  --     , Attr.height "48"
  --     , Attr.x (toString (x * 48))
  --     , Attr.y (toString (y * 48))
  --     ]
  --     []
  --   ]
  -- ]
  Svg.svg
  [ Attr.width "48"
  , Attr.height "48"
  , Attr.viewBox "0 0 48 48"
  ]
  [ Svg.image
    [ Attr.xlinkHref ctx.path
    , Attr.viewBox "0 0 48 48"
    , Attr.width "48"
    , Attr.height "48"
    , Attr.x (toString (x * 48))
    , Attr.y (toString (y * 48))
    ]
    []
  ]


viewTile : Context msg -> Int -> Int -> Maybe (Int, Int) -> Svg.Svg msg
viewTile ctx y x maybeCoord =
  Svg.g
   []
   [ Svg.rect
     [ Attr.width "48"
     , Attr.height "48"
     , Attr.x (toString (x * 48))
     , Attr.y (toString (y * 48))
     , Attr.stroke "black"
     --, Attr.fill "grey"
     -- , Attr.fill <|
     --     case maybeCoord of
     --       Nothing -> "grey"
     --       Just coord -> "url(#tile1)"
     , Attr.fill "url(#tile1)"
     , Attr.class "tile"
     , Events.onMouseOut (ctx.onMouseOver x y)
     , Events.onMouseOver (ctx.onMouseOver x y)
     , Events.onClick (ctx.onTileClick x y)
     ]
     []
   -- , case maybeCoord of
   --    Nothing ->
   --      Svg.text ""
   --    Just coord ->
   --      --viewImage ctx coord x y
   --     Svg.text ""
   ]



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
    [ Attr.class "tile-map"
    , ctx.onMouseDown
    ]
    [
     Svg.defs
      []
      [ Svg.pattern
        [ Attr.id "tile1"
        , Attr.width <| toString (48 // 4)
        --, Attr.width "48"
        --, Attr.height "48"
        , Attr.height <| toString (48 // 2)
        , Attr.patternUnits "userSpaceOnUse"
        , Attr.patternTransform "scale(4 4)"
        ]
        [ Svg.image
          [ Attr.xlinkHref ctx.path
          , Attr.width "48"
          , Attr.height "48"
          ]
          []
        ]
      ]
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
  }
