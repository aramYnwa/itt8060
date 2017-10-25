(*

  ITT8060 -- Advanced Programming 2017
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Bind, option, list

  ------------------------------------
  Name: Aramais Khachatryan
  Student ID: arkhac@ttu.ee
  ------------------------------------


  Answer the questions below. You answers to the questions should be
  correct F# code written after the question. This file is an F# script
  file; it should be possible to load the whole file at once. If you
  can't, then you have introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as file
  coursework5.fsx in directory coursework5.

  The deadline for completing the above procedure is Friday,
  October 27, 2017.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// 1. Write a function by pattern matching
// 
//   flattenOption : option<option<'a>> -> option<'a>
//
//   which squashes two layers of possible successes or failures into 1
//   E.g. Some Some 1 -> Some 1

    let flattenOption (arg :option<option<'a>> ) :option<'a> =
      match arg with
      | Some a  -> a
      | None    -> None


// 2. Write a function
//
//    defeatist : list<option<'a>> -> option<list<'a>>
//
//    that takes a list of possible successes or failures and returns
//    a list of successes if everything succeeded or returns failure
//    if 1 or more elements of the list was a failure. Again, pay
//    close attention to the type.
//    E.g. [Some 1 ; Some 2] -> Some [1; 2]

    let defeatist (l :list<option<'a>>) :option<list<'a>> =
      let chosen = List.choose id l
      let len = l.Length
      if len = chosen.Length then Some chosen else None

// 3. Write a function
//
//    optimist : 'a -> list<option<'a>> -> list<'a>
//
//    which collects a list of possible successes or failures into a
//    list containing only the successes with all failures replaced
//    by the first parameter of the function. Pay close attention to the type.
//    E.g. optimist 0 [Some 1; None] -> [1; 0]

    let optimist (a: 'a) (l :List<option<'a>>) : List<'a> =
      List.map (Option.fold (fun acc x -> x) a) l 

// 4. Write a function
//
//    chars : list<string> -> list<char>
//
//    This function should use List.collect (bind) and have the
//    following behaviour:
//    ["hello";"world"] -> ['h';'e';'l';'l';'o';'w';'o';'r';'l';'d']

    let chars (l :List<string>) :List<char> =
        List.collect (fun x -> [for c in x -> c]) l
    
    //Reference to http://www.fssnip.net/5u/title/String-explode-and-implode.

// 5. Write a function
//
//    iprint : list<int> -> string
//
//    This function should use List.foldBack and have the following behaviour:
//    [1 .. 5] |-> "1,2,3,4,5,"
    let iprint (l :List<int>) :string =
      List.foldBack(fun x acc -> x.ToString() + "," + acc) l ""