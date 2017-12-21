module MazeSuite exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Maze exposing (..)
import Debug exposing (..)

maze1 =
  ["#####"
  ,"#   #"
  ,"  #  "
  ,"### #"
  ,"#####"]

suite : Test
suite =
    describe "The Maze module"
      [ test "searches a maze" <|
          \_ ->
                let solved = Maze.solve maze1
                in Expect.equal [(2,0),(2,1),(1,1),(1,2),(1,3),(2,3),(2,4)] solved
    ]
