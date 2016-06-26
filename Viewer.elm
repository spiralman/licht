module Viewer exposing ( Model, Msg, init, update, view, subscriptions )

import AnimationFrame
import Debug exposing (log)
import Html exposing (Html)
import Html.Attributes exposing ( width, height )
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (..)
import Math.Vector3 exposing (..)
import Mouse
import Task
import Time exposing (Time)
import WebGL exposing (..)
import Window


-- Model

type alias Center = { x : Float, y : Float }

type alias Model =
  { imageUrl : String
  , dragging : Bool
  , centerPos : Center
  , dragStart : Mouse.Position
  , posStart : Center
  , size : Window.Size
  }

init : (Model, Cmd Msg)
init =
  ( { imageUrl = ""
    , dragging = False
    , centerPos = { x = 0, y = 0 }
    , dragStart = { x = 0, y = 0 }
    , posStart = { x = 0, y = 0 }
    , size = { width = 0, height = 0 }
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


newCenter : Model -> Mouse.Position -> Center
newCenter model {x, y} =
  let
    dx = toFloat (model.dragStart.x - x)
    dy = toFloat (model.dragStart.y - y)
    scale = 4 / toFloat model.size.height
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

perspective : Center -> Window.Size -> Mat4
perspective center size =
  let
    w = toFloat size.width
    h = toFloat size.height
    a = w / h
  in
      List.foldr mul Math.Matrix4.identity
        [ makeOrtho2D (-2 * a) (2 * a) -2 2
        , makeLookAt (vec3 center.x center.y 0) (vec3 center.x center.y 1) (vec3 0 1 0)
        ]

view : Model -> Html Msg
view model =
  WebGL.toHtml [ width model.size.width, height model.size.height ]
    [ render
        vertexShader
        fragmentShader
        mesh
        { perspective = perspective model.centerPos model.size } ]

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
