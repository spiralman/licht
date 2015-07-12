import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp

main =
  StartApp.start { model = init, view = view, update = update }


-- Model

type alias AppState =
  { nextID : Int
  , layers : List ( ID, String )
  }

type alias ID = Int

init : AppState
init =
  { layers = [(0, "one"), (1, "two")]
  , nextID = 2
  }


-- Views

view : Signal.Address Action -> AppState -> Html
view address model =
  div []
        [ ul []
             (List.map (viewLayer address) model.layers)
        , button [ onClick address Add ] [ text "+" ]
        ]

viewLayer : Signal.Address Action -> ( ID, String ) -> Html
viewLayer address (id, layer) =
  li []
     [ text layer
     , button [ onClick address (Remove id) ] [ text "x" ] ]


-- Actions

type Action
  = Add
  | Remove ID

update : Action -> AppState -> AppState
update action model =
  case action of
    Add -> { model | layers <- model.layers ++ [(model.nextID, "another")]
                   , nextID <- model.nextID + 1 }
    Remove id -> { model | layers <- List.filter (\ (layerID, _) -> layerID /= id) model.layers }
