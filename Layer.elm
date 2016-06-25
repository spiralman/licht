module Layer exposing ( Model, Msg, init, update, view )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- Model

type alias Model =
  { name : String
  }

init : String -> Model
init layerName =
  { name = layerName
  }


-- Update

type Msg
  = Rename String

update : Msg -> Model -> Model
update msg model =
  case msg of
      Rename newName ->
        { model | name = newName }


-- View

view : Model -> Html Msg
view model =
  div []
    [ text model.name ]
