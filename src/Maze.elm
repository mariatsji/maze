module Maze exposing (..)

import List.Extra exposing (getAt, init)

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

last : Path -> Point
last p = Maybe.withDefault (-1,-1) <| List.head (List.reverse p)

right : Maze -> Path -> Point
right m p = let (c,r) = last p in (c + 1, r)

down : Maze -> Path -> Point
down m p = let (c,r) = last p in (c, r + 1)

left : Maze -> Path -> Point
left m p = let (c,r) = last p in (c - 1, r)

up : Maze -> Path -> Point
up m p = let (c,r) =  last p in (c, r - 1)

search : Maze -> Path -> Paths
search m p = MazeNode (if last p == (-1,-1) then entrance else last p) (search1 m (p ++ [right m p])) (search1 m (p ++ [down m p])) (search1 m (p ++ [left m p])) (search1 m (p ++ [up m p]))

search1 : Maze -> Path -> Paths
search1 m p =
  if foundExit m p then Exit (last p)
  else if hasBeenThere m p then Deadend (last p)
  else if insideMaze m (last p) && isEmpty m (last p) then search m p
  else Deadend (last p)

solution : Paths -> Path -> Path
solution paths p =
  case paths of Exit n -> [n]
                MazeNode n r d l u ->
                  if not <| List.isEmpty <| solution r p then n :: solution r p
                  else if not <| List.isEmpty <| solution d p then n :: solution d p
                  else if not <| List.isEmpty <| solution l p then n :: solution l p
                  else if not <| List.isEmpty <| solution u p then n :: solution u p
                  else []
                Deadend n -> []

hasBeenThere : Maze -> Path -> Bool
hasBeenThere m p = case p of
  [] -> False
  (x::xs) -> if last p == x then True else hasBeenThere m xs

foundExit : Maze -> Path -> Bool
foundExit m path = let (c,r) = last path in (c,r) == exit

isEmpty : Maze -> Point -> Bool
isEmpty maze (r,c) = symbolAt maze (r,c) == ' '

symbolAt : Maze -> Point -> Char
symbolAt maze (r,c) = case getAt r maze of
    Just r1 -> case getAt c (String.toList r1) of
        Just c1 -> c1
        Nothing -> '?'
    Nothing -> '?'

insideMaze : Maze -> Point -> Bool
insideMaze maze (r,c) = r < List.length maze && r >= 0 && c < width maze && c >= 0

width : Maze -> Int
width s = case List.head s of Just r -> List.length (String.toList r)
                              Nothing -> 0

defaultMaze : Maze
defaultMaze =
  ["#####"
  ,"# # #"
  ,"  #  "
  ,"#   #"
  ,"#####"]
