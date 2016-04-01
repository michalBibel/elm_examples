{-----------------------------------------------------------------

A "Tree" represents a binary tree. A "Node" in a binary tree
always has two children. A tree can also be "Empty". Below I have
defined "Tree" and a number of useful functions.

This example also includes some challenge problems :)

-----------------------------------------------------------------}

import Graphics.Element exposing (..)
import Text


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


empty : Tree a
empty =
    Empty


singleton : a -> Tree a
singleton v =
    Node v Empty Empty


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
      Empty ->
          singleton x

      Node y left right ->
          if x > y then
              Node y left (insert x right)

          else if x < y then
              Node y (insert x left) right

          else
              tree


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


depth : Tree a -> Int
depth tree =
    case tree of
      Empty -> 0
      Node v left right ->
          1 + max (depth left) (depth right)


map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
      Empty -> Empty
      Node v left right ->
          Node (f v) (map f left) (map f right)

sum : Tree number -> number
sum tree = 
  case tree of
    Empty -> 0
    Node el left right -> 
      el + sum left + sum right

flatten : Tree a -> List a
flatten tree = 
  case tree of
    Empty -> []
    Node el left right -> 
      flatten left ++ (el :: []) ++ flatten right
      
isElement : comparable -> Tree comparable -> Bool
isElement s tree =
  case tree of
    Empty -> False
    Node cur left right ->
      if s > cur then        
        isElement s right
      else if s < cur then    
        isElement s left
      else
        True
        
fold : (a -> b -> b) -> b -> Tree a -> b
fold f a tree =
  case tree of
    Empty -> a    
    Node v left right -> 
      fold f (fold f (f v a) left) right

sumfold : Tree number -> number
sumfold tree = fold (+) 0 tree

flattenFold : Tree a -> List a
flattenFold tree = fold (::) [] tree

isElementFold : comparable -> Tree comparable -> Bool
isElementFold s tree = 
  let
    eq el prev = prev || el == s
  in
    fold eq False tree

t1 = fromList [1,2,3]
t2 = fromList [2,1,3]

main : Element
main =
    flow down
        [ display "depth" depth t1
        , display "depth" depth t2
        , display "map ((+)1)" (map ((+)1)) t2
        , display "sum" sum t1
        , display "flatten" flatten t2
        , display "isElement" (isElement 4) t2
        , display "isElement" (isElement 3) t2
        , display "sumfold" sumfold t1
        , display "flattenFold" flattenFold t2
        , display "isElementFold" (isElementFold 3) t2
        , display "isElementFold" (isElementFold 4) t2
        ]


display : String -> (Tree a -> b) -> Tree a -> Element
display name f value =
    name ++ " (" ++ toString value ++ ") &rArr;\n    " ++ toString (f value) ++ "\n "
        |> Text.fromString
        |> Text.monospace
        |> leftAligned


{-----------------------------------------------------------------

Exercises:

(1) Sum all of the elements of a tree.

       sum : Tree number -> number

(2) Flatten a tree into a list.

       flatten : Tree a -> List a

(3) Check to see if an element is in a given tree.

       isElement : a -> Tree a -> Bool

(4) Write a general fold function that acts on trees. The fold
    function does not need to guarantee a particular order of
    traversal.

       fold : (a -> b -> b) -> b -> Tree a -> b

(5) Use "fold" to do exercises 1-3 in one line each. The best
    readable versions I have come up have the following length
    in characters including spaces and function name:
      sum: 16
      flatten: 21
      isElement: 46
    See if you can match or beat me! Don't forget about currying
    and partial application!

(6) Can "fold" be used to implement "map" or "depth"?

(7) Try experimenting with different ways to traverse a
    tree: pre-order, in-order, post-order, depth-first, etc.
    More info at: http://en.wikipedia.org/wiki/Tree_traversal

-----------------------------------------------------------------}
