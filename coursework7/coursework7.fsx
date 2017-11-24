(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 7: Property based testing

  ------------------------------------------------
  Name:
  Student ID:
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as
  file coursework7.fsx in directory coursework7.

  The file that should be compiled to a dll should go into coursework7.fs.

  The code under test should go into Tree.fsi, Tree.fs, Vector.fsi and Vector.fs files.

  Please do not upload DLL-s. Just include a readme.txt file containing the 
  dependencies required (additional DLLs).

  The deadline for completing the above procedure is Friday, November 24, 2017.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(* 
   Assumes that FsCheck.dll is in the same directory as the script.
   NB! Do not commit dll files to the solution directories!
*)
#r "FsCheck"

open FsCheck

let revRevIsOrigInt (xs : int list) = List.rev (List.rev xs ) = xs

Check.Quick revRevIsOrigInt
Check.Verbose revRevIsOrigInt


let revIsOrigInt (xs: int list) = List.rev xs = xs

Check.Quick revIsOrigInt
Check.Verbose revIsOrigInt

(*
    Task 1:

    Define FsCheck properties for the following statements:

      * Reversing two lists xs and ys and concatenating the results yields the
        same value as concatenating ys and xs and reversing the result.
        The property should be called
        twoListConcatenationOrderPreservation

      * Taking the lengths of a list of lists xs and adding the individual lengths
        yields the same value as concatenating all sublists of xs and taking
        the length of the result. The property should be called 
        lengthOfListOfListsPreservedInConcatenation

*)

let twoListConcatenationOrderPreservation (xs : int list) (ys: int list) = 
    List.concat [List.rev xs; List.rev ys] = List.rev (List.concat [xs;ys])

Check.Quick twoListConcatenationOrderPreservation

let lengthOfListOfListsPreservedInConcatenation (xs : int list list) = 
    List.length xs + List.fold (fun acc x -> acc + List.length x) 0 xs =
        List.length (List.concat xs)

Check.Quick lengthOfListOfListsPreservedInConcatenation

(*
    Task 2:

    A palindrome is a list that is equal to its reverse. Below you find a
    function isPalindrome, which checks whether a given list is a palindrome.

     a) Define an FsCheck property that expresses the above definition of a
        palindrome. Use the operator ==> for defining your property.
        The property should be called
        palindromeIsEqualToItsReverse

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested. Also show the proportion of trivial inputs.
        The property should be called
        palindromeIsEqualToItsReverseWithStats
*)

let rec isPalindrome xs =
  match xs with
    | []        -> true
    | (x :: xs) -> match List.rev xs with
                     | []        -> true
                     | (y :: ys) -> x = y && isPalindrome ys

let palindromeIsEqualToItsReverse xs =
    isPalindrome xs ==> isPalindrome (List.rev xs)

Check.Quick palindromeIsEqualToItsReverse

let palindromList = Arb.from<list<int>> |> Arb.mapFilter List.sort isPalindrome

let palindromeIsEqualToItsReverseWithStats =
    Prop.forAll palindromList (fun xs -> isPalindrome (List.rev xs))

Check.Quick palindromeIsEqualToItsReverseWithStats

let palindromeIsEqualToItsReverseWithTrivial xs = 
    palindromeIsEqualToItsReverse xs |> Prop.trivial (List.length xs = 0)

Check.Quick palindromeIsEqualToItsReverseWithTrivial

(*
    Task 3:

    Below you find a function toPalindrome, which converts a given list into a
    palindrome of the same length, more or less by replacing the second half of
    the list with the reverse of the first half.

     a) Define an FsCheck property that expresses the definition of a
        palindrome from the previous task. This time, make sure that FsCheck
        does not generate arbitrary lists from which it uses only the
        palindromes, but generates palindromes directly.
        The property should be called
        palindromeIsEqualToItsReverseWithGen

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested. Also show the proportion of trivial inputs.
        The property should be called
        palindromeIsEqualToItsReverseWithGenAndStats

*)

let toPalindrome xs =
  let len       = List.length xs
  let suffixLen = len / 2
  let prefixLen = if 2 * suffixLen = len then suffixLen else suffixLen + 1
  let take n xs = Seq.toList (Seq.take n xs)
  take prefixLen xs @ List.rev (take suffixLen xs)


let getPalindrom xs =
    gen {
        return toPalindrome xs 
        }

let palindromeIsEqualToItsReverseWithGen xs= 
    Prop.forAll (Arb.fromGen (getPalindrom xs)) isPalindrome |> Prop.trivial (List.length xs = 0)

Check.Quick palindromeIsEqualToItsReverseWithGen



(*
    Task 4:

    Copy all the code into coursework7.fs file, use the appropriate [<TestFixture>],
    [<Property>] attributes so that the tests are runnable using FsCheck.NUnit
    (v. 3.0.0, use the pre-release library).

*)

(*  Task 5:

    Take the tree module and vector module from the lecture notes (lecture 11) and write unit and property based tests
    to the extent that you consider the code being reasonably well tested.
    Include the tests in coursework7.fs file. The module code whould go to Tree.fsi, Tree.fs, Vector.fsi and Vector.fs
    files.

*)