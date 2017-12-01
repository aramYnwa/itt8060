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

let tree1 = Branch (Branch(Leaf 5.0, Leaf 4.0), Leaf 10.0)
let tree = Branch (
            Branch (
                Leaf 9.6,
                Leaf 2.3),
            Branch (
                Branch(
                    Leaf 6.7,
                    Leaf 5.9),
                Leaf 4.7))

let rec minInTree (t :float Tree) (cont) :float = 
  match t with 
  | Leaf a -> cont a  
  | Branch (l,r) -> minInTree l (fun left -> minInTree r (fun right -> cont (min left right)))


minInTree tree
minInTree tree1
(*
  Task 4:

  Write a function minInTree' : int Tree -> int that returns the minimum label
  in the given tree, like the function minInTree from Task 3 does. Use
  continuation-passing style in combination with accumulation in your
  implementation.
*)

let minInTree' (t :int Tree) : int =
  let rec minlabel (t :int Tree) (acc :int) (c) :int =
    match t with 
    | Leaf a ->
       let acc = a
       acc
    | Branch (l, r) ->
       minlabel l acc (fun left -> minlabel r acc (fun right -> c (min left right)))
  minlabel t System.Int32.MaxValue id 



let tree3 = Branch (Branch(Leaf 5, Leaf 4), Leaf 10)
let tree4 = Branch (
            Branch (
                Leaf 9,
                Leaf 2),
            Branch (
                Branch(
                    Leaf 6,
                    Leaf 5),
                Leaf 4))

minInTree' tree3
minInTree' tree4