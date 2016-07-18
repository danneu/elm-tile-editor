
module Main exposing (..)

-- Elm Core
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
-- 3rd
import List.Extra as LE
import Mouse
-- 1st
import Grid exposing (Grid)


-- MODEL


type alias Tileset =
  { tilesize : Int
  , rows : Int
  , cols : Int
  , path : String
  }


type alias Drag =
  { start : Mouse.Position
  , current : Mouse.Position
  }


type alias Model =
  { tileset : Tileset
  , selection : (Int, Int)
  , grid : Grid
  , position : Mouse.Position
  , drag : Maybe Drag
  }


init : (Model, Cmd Msg)
init =
  ( { tileset = Tileset 48 2 4 "./img/tileset.png"
    , selection = (0, 0)
    , grid = Grid.empty 10 10
    , drag = Nothing
    , position = Mouse.Position 0 0
    }
  , Cmd.none
  )


-- UPDATE


type Msg
  = NoOp
  | TileSelector Int Int
  | Zoom
  | TileClick Int Int
  -- DRAG
  | DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let _ = Debug.log "msg" msg in
  case msg of
    NoOp ->
      (model, Cmd.none)
    Zoom ->
      (model, Cmd.none)
    TileSelector x y ->
      { model
          | selection = (x, y)
      } ! [Cmd.none]
    TileClick x y ->
      (model, Cmd.none)
    -- DRAG
    DragStart xy ->
      { model
          | drag = Just (Drag xy xy)
      } ! [Cmd.none]
    DragAt xy ->
      { model
          | drag = Maybe.map (\ {start} -> Drag start xy) model.drag
      } ! [Cmd.none]
    DragEnd _ ->
      { model
          | drag = Nothing
          , position = getPosition model
      } ! [Cmd.none]


getPosition : Model -> Mouse.Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position
    Just {start, current} ->
      Mouse.Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)


-- VIEW


viewTileset : Model -> Html Msg
viewTileset ({tileset} as model) =
  let
    positionX x = -x * tileset.tilesize
    positionY y = -y * tileset.tilesize
    viewTile y x =
      li
      [ classList [ ("tile", True)
                  , ("selected", (x, y) == model.selection)
                  ]
      , style [ ("width", toString tileset.tilesize ++ "px")
              , ("height", toString tileset.tilesize ++ "px")
              , ("background-image", "url(" ++ tileset.path ++ ")")
              , ("background-position",
                toString (positionX x) ++ "px " ++ toString (positionY y) ++ "px")
              ]
      , onClick (TileSelector x y)
      ]
      []
    viewRow y =
      ul
      [ class "list-inline"
      , style [ ("height", toString tileset.tilesize ++ "px")
              ]
      ]
      (List.map (viewTile y) [0 .. tileset.cols - 1])
  in
    div
    [ class "tile-selector"
    , style [ ("width", toString (tileset.tilesize * tileset.cols) ++ "px")
            ]
    ]
    (List.map viewRow [0 .. tileset.rows - 1])




view : Model -> Html Msg
view ({tileset} as model) =
  div
  [ class "container" ]
  [ p
    []
    [ pre [] [ text <| toString (model.drag, model.position) ]
    , viewTileset model
    , let
        ctx =
          { onTileClick = TileClick
          , onMouseDown = on "mousedown" (JD.map DragStart Mouse.position)
          , offset = getPosition model
          }
      in
        Grid.view ctx model.grid
    ]
  ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch
        [ Mouse.moves DragAt
        , Mouse.ups DragEnd
        ]


main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
