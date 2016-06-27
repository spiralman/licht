module Viewer exposing ( Model, Msg, init, update, view, subscriptions )

import AnimationFrame
import Debug exposing (log)
import Html exposing (Html)
import Html.Attributes exposing ( width, height )
import Html.Events exposing ( onWithOptions, defaultOptions )
import Json.Decode exposing ((:=))
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (..)
import Math.Vector3 exposing (..)
import Mouse
import Task
import Time exposing (Time)
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
  { imageUrl : String
  , dragging : Bool
  , centerPos : Center
  , dragStart : Mouse.Position
  , posStart : Center
  , size : Window.Size
  , scale : Float
  }

init : (Model, Cmd Msg)
init =
  ( { imageUrl = ""
    , dragging = False
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
  = ChangeImageUrl String
  | Tick Time
  | PosChange Mouse.Position
  | DragStart Mouse.Position
  | DragStop Mouse.Position
  | Resize Window.Size
  | ChangeZoom ScrollEvent


update : Msg -> Model -> Model
update msg model =
  case msg of
      ChangeImageUrl newUrl ->
        { model | imageUrl = newUrl }
      Tick elapsed ->
        model
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
    [ AnimationFrame.diffs Tick
    , Mouse.moves PosChange
    , Mouse.downs DragStart
    , Mouse.ups DragStop
    , Window.resizes Resize
    ]

-- View

type alias Vertex = { position : Vec3, color : Vec3 }

mesh : Drawable Vertex
mesh =
  TriangleFan
    [ Vertex (vec3 -1 1 0) (vec3 1 0 0)
    , Vertex (vec3 -1 -1 0) (vec3 1 0 0)
    , Vertex (vec3 1 -1 0) (vec3 1 0 0)
    , Vertex (vec3 1 1 0) (vec3 1 0 0)
    ]

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

view : Model -> Html Msg
view model =
  WebGL.toHtml [ width model.size.width
               , height model.size.height
               , onWheel ChangeZoom
               ]
    [ render
        vertexShader
        fragmentShader
        mesh
        { perspective = perspective model.centerPos model.scale model.size } ]

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

vertexShader : Shader { attr | position : Vec3, color : Vec3 } { u | perspective : Mat4 } { vcolor : Vec3 }
vertexShader =
  [glsl|

     attribute vec3 position;
     attribute vec3 color;
     uniform mat4 perspective;
     varying vec3 vcolor;

     void main() {
           gl_Position = perspective * vec4(position, 1.0);
           vcolor = color;
     }
  |]

fragmentShader : Shader {} u { vcolor : Vec3 }
fragmentShader =
  [glsl|
     precision mediump float;
     varying vec3 vcolor;
     void main() {
           gl_FragColor = vec4(vcolor, 1.0);
     }
  |]
