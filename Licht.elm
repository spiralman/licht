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
             (List.map (viewLayer address) model.layers)
        , button [ onClick address Add ] [ text "+" ]
        ]

viewLayer : Signal.Address Action -> Layer.Layer -> Html
viewLayer address layer =
  let context =
        Layer.Context
             (Signal.forwardTo address Remove)
  in
    Layer.viewLayer context layer

-- Actions

type Action
  = Add
  | Remove Layer.ID

update : Action -> AppState -> AppState
update action model =
  case action of
    Add -> { model | layers <- model.layers ++ [{ id = model.nextID
                                                , name = "another"}]
                   , nextID <- model.nextID + 1 }
    Remove id -> { model | layers <- List.filter (\layer -> layer.id /= id) model.layers }
