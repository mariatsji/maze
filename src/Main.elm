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
type alias Model = Maze.Maze

model : Model
model = Maze.defaultMaze

-- UPDATE
type Msg = Solve Maze.Maze | Reset

update : Msg -> Model -> Model
update msg mo =
  case msg of Solve maze -> Maze.asMaze maze <| Maze.solve maze
              Reset -> Maze.defaultMaze

-- VIEW

mazeString : Model -> String
mazeString maze = String.fromList <| List.concat <| List.map String.toList <| List.intersperse "\n" maze

toMaze : String -> Maze.Maze
toMaze s = String.lines s

view : Model -> Html Msg
view maz =
  div []
    [ textarea [ rows 5, cols 5, myStyle ] [ text <| mazeString maz ]
    , button [ onClick (Solve maz) ] [ text "Solve" ]
    , button [ onClick Reset ] [ text "reset" ]
    , div [ myStyle ]
      [ textarea [ rows 5, cols 5, myStyle ] [ text <| mazeString maz ] ]
    ]

myStyle =
  style
    [ ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "left")
    , ("font-family", "monospace")
    ]
