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
update msg model =
  case msg of Solve maze -> let solution = Maze.search model [Maze.entrance]
                                solutionPath = Maze.solution solution [Maze.entrance]
                            in (Maze.asMaze model solutionPath)
              Reset -> Maze.defaultMaze

-- VIEW

mazeString : Model -> String
mazeString maze = String.fromList <| List.concat <| List.map String.toList <| List.intersperse "\n" maze

toMaze : String -> Maze.Maze
toMaze s = String.lines s

view : Model -> Html Msg
view model =
  div []
    [ textarea [ rows 5, cols 5, myStyle ] [ text <| mazeString model ]
    , button [ onClick (Solve model) ] [ text "Solve" ]
    , div [ myStyle ] [ text "l8r g8r" ]
    ]

myStyle =
  style
    [ ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "left")
    , ("font-family", "monospace")
    ]
