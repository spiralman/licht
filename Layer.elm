module Layer exposing ( Model, Msg, init, update, view )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- Model

type alias Model =
  { name : String
  , editing : Bool
  }

init : String -> Model
init layerName =
  { name = layerName
  , editing = False
  }


-- Update

type Msg
  = Rename String
  | StartEditing
  | StopEditing

update : Msg -> Model -> Model
update msg model =
  case msg of
      Rename newName ->
        { model | name = newName }
      StartEditing ->
        { model | editing = True }
      StopEditing ->
        { model | editing = False }


-- View

view : Model -> Html Msg
view model =
  div []
    [ if model.editing then (editingView model) else (simpleView model) ]

simpleView : Model -> Html Msg
simpleView model =
  div [ onClick StartEditing ] [ text model.name ]

editingView : Model -> Html Msg
editingView model =
  div []
    [ input [ value model.name, onInput Rename ] []
    , button [ onClick StopEditing ] [ text "Ok" ]
    ]
