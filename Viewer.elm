module Viewer exposing ( Model, Msg, init, update, view, subscriptions )

import Html exposing (Html)
import Html.App as App
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (..)
import Math.Vector3 exposing (..)
import Task
import Viewport2D
import WebGL exposing (..)

-- Model

type alias Model =
  { imageUrl : String
  , viewport : Viewport2D.Model
  , texture : Maybe Texture
  }

init : (Model, Cmd Msg)
init =
  let
    (viewportModel, viewportCmd) = Viewport2D.init
  in
    ( { imageUrl = ""
      , viewport = viewportModel
      , texture = Nothing
      }
    , Cmd.batch
      [ Cmd.map ModifyViewport viewportCmd
      , loadTexture "/images/flower.png"
        |> Task.perform TextureError TextureLoaded
      ]
    )


-- Update

type Msg
  = ChangeImageUrl String
  | ModifyViewport Viewport2D.Msg
  | TextureError Error
  | TextureLoaded Texture


update : Msg -> Model -> Model
update msg model =
  case msg of
      ChangeImageUrl newUrl ->
        { model | imageUrl = newUrl }
      TextureError err ->
        model
      TextureLoaded texture ->
        { model | texture = Just texture }
      ModifyViewport msg ->
        { model | viewport = Viewport2D.update msg model.viewport }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map ModifyViewport (Viewport2D.subscriptions model.viewport)
    ]

-- View

type alias Vertex = { position : Vec3, coord : Vec3 }

mesh : Drawable Vertex
mesh =
  TriangleFan
    [ Vertex (vec3 -1 1 0) (vec3 0 1 0)
    , Vertex (vec3 -1 -1 0) (vec3 0 0 0)
    , Vertex (vec3 1 -1 0) (vec3 1 0 0)
    , Vertex (vec3 1 1 0) (vec3 1 1 0)
    ]

view : Model -> Html Msg
view {texture, viewport} =
  App.map ModifyViewport
    (Viewport2D.view
       (case texture of
            Nothing ->
              []
            Just tex ->
              [
               \perspective ->
                 WebGL.render
                   vertexShader
                   fragmentShader
                   mesh
                   { perspective = perspective
                   , image = tex }
              ]
          )
     viewport)


vertexShader : Shader { attr | position : Vec3, coord : Vec3 } { u | perspective : Mat4 } { vcoord : Vec2 }
vertexShader =
  [glsl|

     attribute vec3 position;
     attribute vec3 coord;
     uniform mat4 perspective;
     varying vec2 vcoord;

     void main() {
           gl_Position = perspective * vec4(position, 1.0);
           vcoord = coord.xy;
     }
  |]

fragmentShader : Shader {} { u | image : Texture } { vcoord : Vec2 }
fragmentShader =
  [glsl|
     precision mediump float;
     uniform sampler2D image;
     varying vec2 vcoord;
     void main() {
           gl_FragColor = texture2D(image, vcoord);
     }
  |]
