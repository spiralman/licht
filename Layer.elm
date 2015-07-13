module Layer where

import Html exposing (..)
import Html.Events exposing (onClick)

-- Model

type alias Layer =
  { id : ID
  , name : String
  }

type alias ID = Int


-- Views

viewLayer : Context -> Layer -> Html
viewLayer context layer =
  li []
     [ text layer.name
     , button [ onClick context.remove layer.id ] [ text "x" ] ]


-- Actions

type alias Context =
  { remove : Signal.Address ID
  }
