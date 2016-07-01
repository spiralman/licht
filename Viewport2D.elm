module Viewport2D exposing ( Model, Msg, init, update, view, subscriptions )

import Html exposing (Html)
import Html.Attributes exposing ( width, height )
import Html.Events exposing ( onWithOptions, defaultOptions )
import Json.Decode exposing ((:=))
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import Mouse
import Task
import WebGL exposing (..)
import Window


-- Model

type alias ScrollEvent =
  { deltaX : Float
  , deltaY : Float
  , deltaZ : Float
  }

type alias Center = { x : Float, y : Float }

type alias Model =
  { dragging : Bool
  , centerPos : Center
  , dragStart : Mouse.Position
  , posStart : Center
  , size : Window.Size
  , scale : Float
  }

init : (Model, Cmd Msg)
init =
  ( { dragging = False
    , centerPos = { x = 0, y = 0 }
    , dragStart = { x = 0, y = 0 }
    , posStart = { x = 0, y = 0 }
    , size = { width = 0, height = 0 }
    , scale = 4
    }
  , Task.perform (always Resize (0, 0)) Resize Window.size
  )

-- Update

type Msg
  = PosChange Mouse.Position
  | DragStart Mouse.Position
  | DragStop Mouse.Position
  | Resize Window.Size
  | ChangeZoom ScrollEvent


update : Msg -> Model -> Model
update msg model =
  case msg of
      PosChange pos ->
        { model
          | centerPos =
            if model.dragging
            then newCenter model pos
            else model.centerPos
        }
      DragStart pos ->
        { model
          | dragging = True
          , dragStart = pos
          , posStart = model.centerPos
        }
      DragStop pos ->
        { model | dragging = False }
      Resize newSize ->
        { model | size = newSize }
      ChangeZoom scroll ->
        { model | scale = model.scale + (scroll.deltaY * 0.01) }


newCenter : Model -> Mouse.Position -> Center
newCenter model {x, y} =
  let
    dx = toFloat (model.dragStart.x - x)
    dy = toFloat (model.dragStart.y - y)
    scale = model.scale / toFloat model.size.height
    dx' = dx * scale
    dy' = dy * scale
  in
      { x = model.posStart.x - dx'
      , y = model.posStart.y - dy'
      }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.moves PosChange
    , Mouse.downs DragStart
    , Mouse.ups DragStop
    , Window.resizes Resize
    ]

-- View

perspective : Center -> Float -> Window.Size -> Mat4
perspective center scale size =
  let
    w = toFloat size.width
    h = toFloat size.height
    a = w / h
    halfScale = scale / 2
  in
      List.foldr mul Math.Matrix4.identity
        [ makeOrtho2D (-halfScale * a) (halfScale * a) -halfScale halfScale
        , makeLookAt (vec3 center.x center.y 0) (vec3 center.x center.y 1) (vec3 0 1 0)
        ]

view : List (Mat4 -> Renderable) -> Model -> Html Msg
view renderers model =
  WebGL.toHtml [ width model.size.width
               , height model.size.height
               , onWheel ChangeZoom
               ]
    (List.map
       (\renderer ->
          renderer (perspective model.centerPos model.scale model.size) )
       renderers)

onWheel : (ScrollEvent -> msg) -> Html.Attribute msg
onWheel tagger =
  onWithOptions "wheel"
    { defaultOptions | preventDefault = True }
    (Json.Decode.map tagger decodeWheel)

decodeWheel : Json.Decode.Decoder ScrollEvent
decodeWheel =
  Json.Decode.object3 ScrollEvent
    ("deltaX" := Json.Decode.float)
    ("deltaY" := Json.Decode.float)
    ("deltaZ" := Json.Decode.float)
