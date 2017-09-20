(*

  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 1: Basic operations on lists

  ------------------------------------
  Name: Aramais Khachatryan
  TUT Student ID: arkhac@ttu.ee
  ------------------------------------


  Answer all the questions below.  You answers to questions should be
  correct F# code written after the question in comments. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere and your result will not be evaluated.

  This coursework will be graded.

  To submit the coursework you will be asked to
  
  1) Check out your  GIT repository
  from the server gitlab.cs.ttu.ee using instructions on page
  https://courses.cs.ttu.ee/pages/ITT8060

  2) Put your solution into a file coursework1/coursework1.fsx
  in the repository. Commit it and push it to the server!
  It is your responsibility to make sure you have pushed the solution
  to the repository!

  NB! It is very important to make sure you use the exact name using
  only small caps. Files submitted under wrong name may not get a grade.

  Also, use the exact function and identifier names with precise types as 
  specified in the question.

 
*)

// 1. Associate an identifier "myFirstList" with an empty list of type 'float list' (or list<float>).
let myFirstList = []: float List

// 2. Write a function
// count 'a list -> int
// that will return a number of elements in a list. 

let rec count = function
    | [] -> 0
    | [h] -> 1
    | head::tail -> 1 + count tail

// 3. Make a list of cantines available on TTU campus containing 4-tuples (quadruples).
// The identifier of the list should be "cantines". The 4-tuples should be of type string * string * int * int
// The elements of the 4-tuples should represent the following:
//   1) The name of the cantine
//   2) The building identifier
//   3) The closing hour as integer
//   4) The closing minutes as ingeger

let cantines = [ ("Libary canteen", "Akadeemia tee 1", 19, 0);
                ("Main building Deli cafe", "Akadeemia tee 5", 16, 30);
                ("Sports buiding canteen", "Maniliiva 7", 20, 0);
                ("ICT building canteen", "Raja 15", 16, 0)]

// 4. Write a function currentlyOpen: int -> int -> (string * string * int * int) list -> string list
// that will return the building identifiers where cantines have not yet closed. The first argument is the
// current hours as integer, the second is the current minutes as integer, the third is the list of cantines 
// with the same type as you specified in previous question.
// Your solution should do the filtering explicitly.

let rec currentlyOpen (curr_hour: int)  (curr_min:int) (l: (string*string*int*int) list) =
    match l with
    | [] -> []
    | (name, loc, hour, min) :: tail when hour > curr_hour -> name :: currentlyOpen curr_hour curr_min tail
    | (name, loc, hour, min) :: tail when hour = curr_hour && min > curr_min -> name :: currentlyOpen curr_hour curr_min tail
    | head :: tail -> currentlyOpen curr_hour curr_min tail


// 5. Write a function currentlyOpen2: int -> int -> (string * string * int * int) list -> string list
// that behaves similarily to the currentlyOpen, but uses List.filter in its implementation.

let currentlyOpen2 (curr_hour:int) (curr_min:int) (l: (string*string*int*int) list) =
     List.filter (fun (name, loc, hour, min) -> 
                    if (hour > curr_hour) || (hour = curr_hour && min > curr_min) then true else false) l
                    |> List.map(fun (name, loc, hour, min) -> name)
