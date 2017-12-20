import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Maze

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- MODEL
type alias Model = Int

model : Model
model = 0

-- UPDATE
type Msg = Solve | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of Solve -> 1
              Reset -> 0

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "textarea", placeholder (toString model) ] []
    , button [ onClick Solve ] [ text "Solve" ]
    , button [ onClick Reset ] [ text "Reset" ]
    ]
