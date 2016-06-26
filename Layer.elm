module Layer exposing ( Model, Msg, init, update, view )

import Color exposing (..)
import Color.Convert exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onInput )

import String exposing ( toFloat )


-- Model

type alias Model =
  { name : String
  , editing : Bool
  , luminanceTolerance : Float
  , chromaTolerance : Float
  , selectColor : Color
  }

init : String -> Model
init layerName =
  { name = layerName
  , editing = True
  , luminanceTolerance = 0.5
  , chromaTolerance = 0.5
  , selectColor = rgb 128 128 128
  }


-- Update

type Msg
  = Rename String
  | StartEditing
  | StopEditing
  | ChangeLuminanceTolerance Float
  | ChangeChromaTolerance Float
  | ChangeSelectColor Color

update : Msg -> Model -> Model
update msg model =
  case msg of
      Rename newName ->
        { model | name = newName }
      StartEditing ->
        { model | editing = True }
      StopEditing ->
        { model | editing = False }
      ChangeLuminanceTolerance newTolerance->
        { model | luminanceTolerance = newTolerance }
      ChangeChromaTolerance newTolerance->
        { model | chromaTolerance = newTolerance }
      ChangeSelectColor newColor ->
        { model | selectColor = newColor }


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
    , colorControl "Selection" model.selectColor ChangeSelectColor
    , toleranceControl "Luminance" model.luminanceTolerance ChangeLuminanceTolerance
    , toleranceControl "Color" model.chromaTolerance ChangeChromaTolerance
    ]

colorControl : String -> Color -> (Color -> Msg) -> Html Msg
colorControl lbl currentValue msg =
  label []
    [ text lbl
    , input [ value (colorToHex currentValue)
            , type' "color"
            , onInput (colorValue msg currentValue)
            ]
        []
    ]

toleranceControl : String -> Float -> (Float -> Msg) -> Html Msg
toleranceControl lbl currentValue msg =
  label []
    [ text lbl
    , input [ value (toString currentValue)
            , type' "range"
            , Html.Attributes.min "0.0"
            , Html.Attributes.max "1.0"
            , step "0.01"
            , onInput (floatValue msg currentValue)
            ]
        []
    ]

floatValue : (Float -> Msg) -> Float -> String -> Msg
floatValue msg default value =
  case String.toFloat value of
      Ok v ->
        msg v
      Err _ ->
        msg default

colorValue : (Color -> Msg) -> Color -> String -> Msg
colorValue msg default value =
  case hexToColor value of
      Just c ->
        msg c
      Nothing ->
        msg default
