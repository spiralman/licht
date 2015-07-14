import Debug
import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp
import Layer

main =
  StartApp.start { model = init, view = view, update = update }


-- Model

type alias AppState =
  { nextID : Layer.ID
  , layers : List Layer.Layer
  }

init : AppState
init =
  { layers = []
  , nextID = 0
  }


-- Views

view : Signal.Address Action -> AppState -> Html
view address model =
  div []
        [ ul []
             (List.map (viewLayer address) (Debug.watch "layers" model.layers))
        , button [ onClick address Add ] [ text "+" ]
        ]

viewLayer : Signal.Address Action -> Layer.Layer -> Html
viewLayer address layer =
  let context =
        Layer.Context
             (Signal.forwardTo address Update)
             (Signal.forwardTo address Remove)
  in
    Layer.viewLayer context layer

-- Actions

type Action
  = Add
  | Update (Layer.ID, Layer.Action)
  | Remove Layer.ID

update : Action -> AppState -> AppState
update action model =
  case action of
    Add -> { model | layers <- model.layers ++ [Layer.newLayer model.nextID]
                   , nextID <- model.nextID + 1 }
    Update (id, action) -> { model | layers <- List.map (\layer ->
                                                      if layer.id == id
                                                      then Layer.updateLayer action layer
                                                      else layer)
                          model.layers }
    Remove id -> { model | layers <- List.filter (\layer -> layer.id /= id) model.layers }
