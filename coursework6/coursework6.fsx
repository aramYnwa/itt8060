(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Tail recursion

  ------------------------------------------------
  Name: Aramais Khachatryan
  Student ID: arkhac@ttu.ee
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is Friday, November 10, 2017.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function minInList : float list -> float that returns the minimum element
  in the given list. Make sure your implementation uses tail recursion.
*)

let minInList (l :float list) :float =
  let rec min (l:float list, m: float) =
    match l with 
    | [] -> m
    | hd :: tl when hd < m -> min(tl, hd)
    | hd :: tl -> min(tl, m)
  min(l, (l.Head))

minInList([3.0; 4.5; -5.6; 9.0])
minInList([])

(*
  Task 2:

  Write a function swapElementsInList : 'a list -> 'a list that swaps the 1st
  element with the second, the 3rd with the 4th, etc. Make sure your
  implementation uses tail recursion.
*)

let swapElementsInList l =
  let rec swap(l, acc) =
    match l with 
    | [] -> acc
    | [hd] -> acc @ [hd]
    | hd :: m :: tl -> swap(tl, acc @ [m] @ [hd])
  swap(l, [])

swapElementsInList [1;2;3;4]
swapElementsInList [1;2;3]
swapElementsInList [1;2]
swapElementsInList [1]

(*
  Task 3:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function minInTree : float Tree -> float that returns the minimum label in the
  given tree. Use continuation-passing style in your implementation.
*)

type 'a Tree =
  | Leaf   of 'a
  | Branch of 'a Tree * 'a Tree

let tree1 = Branch (Branch(Leaf 1.0, Leaf 1.0), Leaf 1.0)
let tree = Branch (
            Branch (
                Leaf 9.6,
                Leaf 2.3),
            Branch (
                Branch(
                    Leaf 6.7,
                    Leaf 5.9),
                Leaf 4.7))


let rec minInTree (t :float Tree) :float = 
  match t with 
  | Leaf _ -> 0.0
  | Branch (l,r) -> (min (minInTree(l)) (minInTree(r))) + 1.0
(*
  Task 4:

  Write a function minInTree' : int Tree -> int that returns the minimum label
  in the given tree, like the function minInTree from Task 3 does. Use
  continuation-passing style in combination with accumulation in your
  implementation.
*)

let minInTree' (t :float Tree) : float =
  let rec minlabel (t :float Tree, acc :float) :float =
    match t with 
    | Leaf _ -> acc 
    | Branch (l, r) ->
       let acc = acc + 1.0
       min (minlabel (l, acc)) (minlabel (r, acc))
  minlabel(t, 0.0)   


minInTree tree
minInTree' tree

minInTree tree1
minInTree' tree1