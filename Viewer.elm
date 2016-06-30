module Viewer exposing ( Model, Msg, init, update, view, subscriptions )

import Html exposing (Html)
import Html.App as App
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import Viewport2D
import WebGL exposing (..)

-- Model

type alias Model =
  { imageUrl : String
  , viewport : Viewport2D.Model
  }

init : (Model, Cmd Msg)
init =
  let
    (viewportModel, viewportCmd) = Viewport2D.init
  in
    ( { imageUrl = ""
      , viewport = viewportModel
      }
    , Cmd.map ModifyViewport viewportCmd
    )


-- Update

type Msg
  = ChangeImageUrl String
  | ModifyViewport Viewport2D.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
      ChangeImageUrl newUrl ->
        { model | imageUrl = newUrl }
      ModifyViewport msg ->
        { model | viewport = Viewport2D.update msg model.viewport }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map ModifyViewport (Viewport2D.subscriptions model.viewport)
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

view : Model -> Html Msg
view model =
  App.map ModifyViewport
    (Viewport2D.view [
        WebGL.render vertexShader fragmentShader mesh
       ]
       model.viewport)


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
