import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Layer

main =
  App.beginnerProgram { model = model, view = view, update = update }


-- Model

type alias Model =
  { layers : List IndexedLayer
  , nextUid : Int
  }

type alias IndexedLayer =
  { id : Int
  , model : Layer.Model
  }

model : Model
model =
  { layers = []
  , nextUid = 0
  }



-- Update

type Msg
  = Insert
  | Remove
  | Modify Int Layer.Msg

update : Msg -> Model -> Model
update msg ({layers, nextUid} as model) =
  case msg of
      Insert ->
        { model
          | layers = layers ++ [ IndexedLayer
                                   nextUid
                                   (Layer.init ("Layer " ++ (toString nextUid)))
                               ]
          , nextUid = nextUid + 1
        }

      Remove ->
        { model | layers = List.drop 1 layers }

      Modify id msg ->
        { model | layers = List.map (updateIndexed id msg) layers }

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
    ]

viewIndexedLayer : IndexedLayer -> Html Msg
viewIndexedLayer {id, model} =
  App.map (Modify id) (Layer.view model)
