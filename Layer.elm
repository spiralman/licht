module Layer where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue, onBlur, keyCode)

import Json.Decode as Json


-- Model

type alias Layer =
  { id : ID
  , name : String
  , editing : Bool
  }

type alias ID = Int

newLayer : ID -> Layer
newLayer id =
  { id = id
  , name = ""
  , editing = True
  }


-- Views

viewLayer : Context -> Layer -> Html
viewLayer context layer =
  li [ onClick context.update (layer.id, Editing True)]
     [ if layer.editing
       then input
              [ placeholder "Layer Name"
              , autofocus True
              , value layer.name
              , on "input" targetValue (\v -> Signal.message context.update (layer.id, Rename v))
              , onBlur context.update (layer.id, Editing False)
              , onEnter context.update (layer.id, Editing False)
              ]
              []
       else text layer.name
     , button [ onClick context.remove layer.id ] [ text "x" ] ]

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
  on "keydown"
       (Json.customDecoder keyCode is13)
       (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


-- Actions

type Action
  = Rename String
  | Editing Bool

type alias Context =
  { update : Signal.Address (ID, Action)
  , remove : Signal.Address ID
  }

updateLayer : Action -> Layer -> Layer
updateLayer action layer =
  case action of
    Rename name -> { layer | name <- name }
    Editing editing -> { layer | editing <- editing }
