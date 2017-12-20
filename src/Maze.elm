module Maze exposing (..)

import List.Extra

type alias Row = Int
type alias Col = Int
type alias Point = (Row, Col)
type alias Path =  List Point
type alias Maze =  List (String)
type Paths = MazeNode Point Paths Paths Paths Paths | Deadend Point | Exit Point

entrance : Point
entrance = (2,0)

exit : Point
exit = (2,4)
