
module Main exposing (..)

-- Elm Core
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Task
-- 3rd
import List.Extra as LE
import Mouse
import Keyboard.Extra as KE
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


type Mode
  = Paint -- Dragging will fill in tiles over an area
  | Move -- Dragging will move the tile map


type alias Model =
  { tileset : Tileset
  , selection : (Int, Int)
  , grid : Grid
  , position : Mouse.Position
  , drag : Maybe Drag
  , mode : Mode
  , kb : KE.Model
  , undoStack : List Grid
  , scale : Float
  }


init : (Model, Cmd Msg)
init =
  let
    (kbModel, kbCmd) = KE.init
  in
    ( { tileset = Tileset 48 2 4 "./img/tileset.png"
      , selection = (0, 0)
      , grid = Grid.empty 10 20
      , drag = Nothing
      , position = Mouse.Position 100 100
      , mode = Move
      , kb = kbModel
      , undoStack = []
      , scale = 1
      }
    , Cmd.map Keyboard kbCmd
    )


-- UPDATE


type Msg
  = NoOp
  | TileSelector Int Int
  | Zoom
  | TileClick Int Int
  -- MODE
  | ChangeMode Mode
  | PaintTile Int Int
  -- DRAG
  | DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position
  -- KEYBOARD
  | Keyboard KE.Msg
  -- UNDO
  | Undo
  -- ZOOM
  | ZoomOut
  | ZoomIn


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
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
      case model.mode of
        Move -> (model, Cmd.none)
        Paint -> update (PaintTile x y) model
    ChangeMode mode ->
      { model
          | mode = mode
      } ! [Cmd.none]
    PaintTile x y ->
      -- No-op if we already selected this tile to avoid junk states
      -- in the undo stack
      if Grid.getTile x y model.grid == (Just model.selection) then
        (model, Cmd.none)
      else
        let
          grid' = Grid.setTile x y (Just model.selection) model.grid
        in
          { model
              | grid = grid'
              , undoStack = List.take 500 (grid' :: model.undoStack)
          } ! [Cmd.none]
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
          , position =
              -- Only update position if we're in Move mode
              case model.mode of
                Move -> getPosition model
                _ -> model.position
      } ! [Cmd.none]
    -- TODO: Figure out intended usage of this library instead of stuffing
    -- everything in this action
    Keyboard kbMsg ->
      let
        (kbModel', kbCmd') = KE.update kbMsg model.kb
        -- Spacebar toggles mode
        mode' =
          if KE.isPressed KE.Space kbModel' then
            case model.mode of
              Paint -> Move
              Move -> Paint
          else
            model.mode
        -- Super-Z rewinds from undo stack
        undoMsg =
          if KE.isPressed KE.Super kbModel' && KE.isPressed KE.CharZ kbModel' then
            Undo
          else
            NoOp
      in
        { model
            | kb = kbModel'
            , mode = mode'
        } ! [Cmd.map Keyboard kbCmd'
            , Task.perform identity identity (Task.succeed undoMsg)
            ]
    Undo ->
      case model.undoStack of
        [] ->
          (model, Cmd.none)
        grid' :: stack' ->
          { model
              | grid = grid'
              , undoStack = stack'
          } ! [Cmd.none]
    ZoomIn ->
      { model
          | scale = Basics.max 0 (model.scale + 0.1)
      } ! [Cmd.none]
    ZoomOut ->
      { model
          | scale = Basics.max 0 (model.scale - 0.1)
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
  [ class <| "container " ++ toString model.mode ++ "-mode"
  ]
  [ div
    [ style [ ("position", "absolute")
            , ("top", "10px")
            , ("left", "10px")
            ]
    ]
    [ span [] [ text "Source: " ]
    , a
      [ href "https://github.com/danneu/elm-tile-editor"
      , style [ ("color", "#2ecc71") ]
      ]
      [ text "danneu/elm-tile-editor" ]
    ]
  , div
    [ class "sidebar" ]
    [ div
      [ class "sidebar-panel" ]
      [ h3 [] [ text "Drag Mode" ]
      , p [] [ small [] [ code [] [text "space"], text " toggles" ] ]
      , div
        [ class "btn-group btn-group-justified"
        ]
        [ div
          [ class "btn-group "]
          [ button
            [ classList [ ("btn", True)
                        , ("btn-default", model.mode /= Move)
                        , ("btn-primary", model.mode == Move)
                        , ("active", model.mode == Move)
                        ]
            , onClick (ChangeMode Move)
            ]
            [ span
              [ class "glyphicon glyphicon-move" ]
              []
            , text " Move"
            ]
          ]
        , div
          [ class "btn-group" ]
          [ button
            [ classList [ ("btn", True)
                        , ("btn-default", model.mode /= Paint)
                        , ("btn-primary", model.mode == Paint)
                        , ("active", model.mode == Paint)
                        ]
            , onClick (ChangeMode Paint)
            ]
            [ span
              [ class "glyphicon glyphicon-tint" ]
              []
            , text " Paint"
            ]
          ]
        ]
      ]
    , div
      [ class "sidebar-panel" ]
      [ h3 [] [ text "Tile Picker" ]
      , viewTileset model
      ]
    , div
      [ class "sidebar-panel" ]
      [ h3
        []
        [ text "Undo Stack "
        , small [] [ text <| " " ++ toString (List.length model.undoStack) ++ " deep" ]
        ]
      , button
        [ onClick Undo
        , class "btn btn-default btn-block"
        ]
        [ text "Undo "
        , span [] [ code [] [text "(cmd-z)"] ]
        ]
      ]
    , div
      [ class "sidebar-panel" ]
      [ h3
        []
        [ text "Zoom" ]
      , button
        [ onClick ZoomIn
        , class "btn btn-default btn-block"
        ]
        [ text "Zoom In "
        , span [] [ code [] [text "(cmd (+))"] ]
        ]
      , button
        [ onClick ZoomOut
        , class "btn btn-default btn-block"
        ]
        [ text "Zoom Out "
        , span [] [ code [] [text "(cmd (-))"] ]
        ]
      ]
    ]
  , let
      ctx =
        { onTileClick = TileClick
        , onMouseDown = on "mousedown" (JD.map DragStart Mouse.position)
        , path = model.tileset.path
        , tilesize = model.tileset.tilesize
        , rows = model.tileset.rows
        , cols = model.tileset.cols
        , scale = model.scale
        , onMouseOver = \x y ->
            -- Only care about mouseover if we're in paint mode during drag
            case (model.mode, model.drag) of
              (Paint, Just _) ->
                PaintTile x y
              _ ->
                NoOp
        , offset =
            -- Only offset by drag position if we're in Move mode
            case model.mode of
              Move ->
                getPosition model
              _ ->
                model.position
        }
    in
      Grid.view ctx model.grid
  ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map Keyboard KE.subscriptions
    , case model.drag of
        Nothing ->
          Sub.none
        Just _ ->
          Sub.batch
            [ Mouse.moves DragAt
            , Mouse.ups DragEnd
            ]
    ]


main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
