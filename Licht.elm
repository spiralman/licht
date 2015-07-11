import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp

main =
  StartApp.start { model = model, view = view, update = update }

model = ["first", "second"]

view address model =
  ul []
       (List.map (viewLayer address) model
        ++ [ button [ onClick address Remove ] [ text "-" ]
           , button [ onClick address Add ] [ text "+" ]
           ]
       )

viewLayer address layer =
  li []
     [ text layer ]

type Action = Add | Remove

update action model =
  case action of
    Add-> model
    Remove -> model
