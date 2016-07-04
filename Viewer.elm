port module Viewer exposing ( Model, Msg, init, update, view, subscriptions, loadImage, imageLoaded )

import Html exposing (Html)
import Html.App as App
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (..)
import Math.Vector3 exposing (..)
import Task
import Viewport2D
import WebGL exposing (..)

-- Model

type alias Vertex = { position : Vec3, coord : Vec3 }

type alias Tile =
  { mesh : Drawable Vertex
  , texture : Texture
  }

type alias Model =
  { imageUrl : String
  , viewport : Viewport2D.Model
  , tiles : List Tile
  }

init : (Model, Cmd Msg)
init =
  let
    (viewportModel, viewportCmd) = Viewport2D.init
  in
    ( { imageUrl = ""
      , viewport = viewportModel
      , tiles = []
      }
    , Cmd.batch
      [ Cmd.map ModifyViewport viewportCmd
      , loadImage "/images/flower.png"
      ]
    )


-- Update

type alias ImageTile =
  { x : Float
  , y : Float
  , url : String
  }

type Msg
  = ChangeImageUrl String
  | ModifyViewport Viewport2D.Msg
  | TextureError Error
  | TextureLoaded Float Float Texture
  | ImageLoaded (List ImageTile)

port loadImage : String -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      ChangeImageUrl newUrl ->
        ({ model | imageUrl = newUrl }, Cmd.none)
      TextureError err ->
        (model, Cmd.none)
      TextureLoaded x y texture ->
        ({ model | tiles = (tile x y texture) :: model.tiles }, Cmd.none)
      ModifyViewport msg ->
        ({ model | viewport = Viewport2D.update msg model.viewport }, Cmd.none)
      ImageLoaded tiles ->
        ( model
        , Cmd.batch
            (List.map
               (\tile -> loadTexture tile.url |> Task.perform TextureError (TextureLoaded tile.x tile.y))
               tiles
            )
        )

port imageLoaded : ((List ImageTile) -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map ModifyViewport (Viewport2D.subscriptions model.viewport)
    , imageLoaded ImageLoaded
    ]

-- View

    -- [ Vertex (vec3 0 1 0) (vec3 1 1 0)
    -- , Vertex (vec3 0 0 0) (vec3 1 0 0)
    -- , Vertex (vec3 1 0 0) (vec3 0 0 0)
    -- , Vertex (vec3 1 1 0) (vec3 0 1 0)

tile : Float -> Float -> Texture -> Tile
tile x y texture =
  let
    x' = -(x / 2048)
    y' = -(y / 2048)
    r = x' + 1
    t = y' - 1
  in
      { mesh =
          TriangleFan
          [ Vertex (vec3 x' t 0) (vec3 1 0 0)
          , Vertex (vec3 x' y' 0) (vec3 1 1 0)
          , Vertex (vec3 r y' 0) (vec3 0 1 0)
          , Vertex (vec3 r t 0) (vec3 0 0 0)
          ]
      , texture = texture
      }

view : Model -> Html Msg
view {viewport, tiles} =
  App.map ModifyViewport
    (Viewport2D.view
       (List.map
          (\tile ->
             (\perspective ->
                WebGL.render
                  vertexShader
                  fragmentShader
                  tile.mesh
                  { perspective = perspective
                  , image = tile.texture
                  }
             )
          )
          tiles
       )
       viewport
    )


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
