import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Layer
import Viewer

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model

type alias Model =
  { layers : List IndexedLayer
  , viewDesc : Viewer.Model
  , nextUid : Int
  }

type alias IndexedLayer =
  { id : Int
  , model : Layer.Model
  }

init : (Model, Cmd Msg)
init =
  ( { layers = []
    , viewDesc = Viewer.init
    , nextUid = 0
    }
  , Cmd.none)


-- Update

type Msg
  = Insert
  | Remove
  | Modify Int Layer.Msg
  | ModifyView Viewer.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({layers, nextUid} as model) =
  case msg of
      Insert ->
        ({ model
           | layers = layers ++ [ IndexedLayer
                                    nextUid
                                    (Layer.init ("Layer " ++ toString nextUid))
                                ]
           , nextUid = nextUid + 1
         }
        , Cmd.none)

      Remove ->
        ({ model | layers = List.drop 1 layers }, Cmd.none)

      Modify id msg ->
        ({ model | layers = List.map (updateIndexed id msg) layers }, Cmd.none)

      ModifyView msg ->
        ({ model | viewDesc = Viewer.update msg model.viewDesc }, Cmd.none)

updateIndexed : Int -> Layer.Msg -> IndexedLayer -> IndexedLayer
updateIndexed targetId msg {id, model} =
  IndexedLayer id (if targetId == id then Layer.update msg model else model)


-- View

view : Model -> Html Msg
view model =
  div []
    [ ul [] (List.map viewIndexedLayer model.layers)
    , button [ onClick Insert ] [ text "+" ]
    , button [ onClick Remove ] [ text "-" ]
    , viewViewer model.viewDesc
    ]

viewViewer : Viewer.Model -> Html Msg
viewViewer model =
  App.map ModifyView (Viewer.view model)

viewIndexedLayer : IndexedLayer -> Html Msg
viewIndexedLayer {id, model} =
  App.map (Modify id) (Layer.view model)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map ModifyView (Viewer.subscriptions model.viewDesc)
    ]
