module Viewer exposing ( Model, Msg, init, update, view, subscriptions )

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing ( width, height )
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (..)
import Math.Vector3 exposing (..)
import Time exposing (Time)
import WebGL exposing (..)


-- Model

type alias Model =
  { imageUrl : String
  }

init : Model
init =
  { imageUrl = ""
  }


-- Update

type Msg
  = ChangeImageUrl String
  | Tick Time

update : Msg -> Model -> Model
update msg model =
  case msg of
      ChangeImageUrl newUrl ->
        { model | imageUrl = newUrl }
      Tick elapsed ->
        model


subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Tick

-- View

type alias Vertex = { position : Vec3, color : Vec3 }

mesh : Drawable Vertex
mesh =
  Triangle
    [ ( Vertex (vec3 -1 1 0) (vec3 1 0 0)
      , Vertex (vec3 -1 -1 0) (vec3 1 0 0)
      , Vertex (vec3 1 -1 0) (vec3 1 0 0)
      )
    ]

perspective : Mat4
perspective =
  List.foldr mul Math.Matrix4.identity
    [ makeOrtho2D -2 2 -2 2
    , makeLookAt (vec3 0 0 0) (vec3 0 0 1) (vec3 0 1 0)
    ]

view : Model -> Html Msg
view model =
  WebGL.toHtml [ width 600, height 600 ]
    [ render vertexShader fragmentShader mesh { perspective = perspective } ]

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
