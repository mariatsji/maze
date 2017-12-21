module Maze exposing (..)

import List.Extra exposing (getAt, init, zip)

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
last p = Maybe.withDefault entrance <| List.head (List.reverse p)

right : Maze -> Path -> Point
right m p = let (r,c) = last p in (r + 1, c)

down : Maze -> Path -> Point
down m p = let (r,c) = last p in (r, c + 1)

left : Maze -> Path -> Point
left m p = let (r,c) = last p in (r - 1, c)

up : Maze -> Path -> Point
up m p = let (r,c) =  last p in (r, c - 1)

search : Maze -> Path -> Paths
search m p = MazeNode (last p) (search1 m (p ++ [right m p])) (search1 m (p ++ [down m p])) (search1 m (p ++ [left m p])) (search1 m (p ++ [up m p]))

search1 : Maze -> Path -> Paths
search1 m p =
  if foundExit m p then Exit (last p)
  else if hasBeenThere p then Deadend (last p)
  else if insideMaze m (last p) && isEmpty m (last p) then search m p
  else Deadend (last p)

solve : Maze -> Path
solve maze = solution (search maze [entrance]) [entrance]

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

hasBeenThere : Path -> Bool
hasBeenThere p = List.member (last p) <| Maybe.withDefault [] (init p)

foundExit : Maze -> Path -> Bool
foundExit m path = let (r,c) = last path in (r,c) == exit

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

asMaze : Maze -> Path -> Maze
asMaze maze p = let chars = coords maze p in List.map (String.fromList) chars

coords : Maze -> Path -> List (List Char)
coords maze p = List.map (\(str, r) -> List.map (\(char, c) -> if List.member (r,c) p then '*' else symbolAt maze (r,c)) (innerZipped str)) (zipped maze)

zipped : Maze -> List (String, Int)
zipped maze = zip maze <| List.range 0 (List.length maze - 1)

innerZipped : String -> List (Char, Int)
innerZipped s = zip (String.toList s) <| List.range 0 (String.length s - 1)

defaultMaze : Maze
defaultMaze =
  ["#####"
  ,"# # #"
  ,"  #  "
  ,"#   #"
  ,"#####"]
