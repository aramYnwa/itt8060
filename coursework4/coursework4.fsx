﻿(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Recursive data types

  ------------------------------------
  Name: Aramais Khachatryan
  Student ID: arkhac@ttu.ee
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060 under your name, into a file
  coursework4/coursework4.fsx by October 20, 2017.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// In this coursework you will be required to design a file system data type
// that is able to carry the information about file names and access rights
// (read and/or write).
// The write permission on a directory is required to create or remove files
// in it, but not to write to an existing file in it.
// You will also be asked to create functions that can carry out various tasks
// on a file system.

// The access permissions are given by the following type:

type Permission = Read | Write | ReadWrite

// 0. Define a function
// createEmptyFilesystem: unit -> FileSystem
// that will be a function with 0 arguments that will return an
// empty filesystem of the type that you defined.
// (Permissions are initially assumed to be ReadWrite, check task 5)  
// We assume that your file system is defined in a type called FileSystem.

type FileSystem = (Element * Permission) list
  and Element = 
  | File of string
  | Dir of string*FileSystem

let createEmptyFilesystem () :FileSystem = 
    //[Dir("root",[])]
    []:FileSystem

let getElement (tpl : Element * Permission) =
    match tpl with
    | (el :Element, p :Permission) -> el
let getName (elem :Element) = 
  match elem with
  | File name -> name
  | Dir (name, content) -> name

let getDirContent (elem: Element) =
   match elem with
   | Dir (name, content) -> content
   //| File name -> raise |> new ArgumentException("")

// 1. Define two functions 
// createFile : string -> FileSystem -> FileSystem
// that will create a file into the root directory of the 
// current file system.
// The first argument is the file name, the second
// is the filesystem to create the file into. 
// (Permissions are initially assumed to be ReadWrite, check task 5)  

(*
let createFile (file :string) (filesystem :FileSystem) :FileSystem =
    let newFile = File file
    let root = filesystem.Head
    [Dir ((getName root), (newFile :: (getDirContent root)))]
*)
let createFile (file :string) (fs :FileSystem) :FileSystem =
    let newFile = File file
    (newFile, ReadWrite) :: fs

// createDir : string -> FileSystem -> FileSystem
// that will create a directory into the root directory of the current
// file system.
// The second argument is the name of the directory 
// (Permissions are initially assumed to be ReadWrite, check task 5)  

(*
let createDir (dirName :string) (filesystem :FileSystem) :FileSystem=
    let newDir = Dir (dirName, [])
    let root = filesystem.Head
    [Dir ((getName root), (newDir :: (getDirContent root)))]
*)
let createDir (dir :string) (fs :FileSystem) :FileSystem =
    let newDir = Dir (dir, [])
    (newDir, ReadWrite) :: fs

// 2. Define a function 
// createSubDir : string -> FileSystem -> FileSystem -> FileSystem
// that will create a directory with name given in the first argument and
// contents given as the second argument into a file system given
// as the third argument.

(*
let createSubDir (dirName :string) (fs1 :FileSystem) (fs2 :FileSystem) :FileSystem =
    let newFS = Dir (dirName, getDirContent fs1.Head)
    let root = fs2.Head
    [Dir((getName root), (newFS :: (getDirContent root)))]
    *)

let createSubDir (dirName :string) (fs1 :FileSystem) (fs2 :FileSystem) :FileSystem =
    let newFS = Dir(dirName, fs2)
    (newFS, ReadWrite) :: fs1

(*    
let createSubDir (dirName :string) (fs1 :FileSystem) (fs2 :FileSystem) :FileSystem = 
    [Dir ((getName fs1), 
          [Dir(dirName, getDirContent fs2)] :: getDirContent fs1), ReadWrite]    
*)

// 3. Define a function
// count : FileSystem -> int
// that will recursively count the number of files in the current filesystem.
(*
let count (fs :FileSystem) = 
    let root = fs.Head
    let FSContent = getDirContent root
    let rec fileCount (rootFS :FileSystem) =
        match rootFS with
        | [] -> 0
        | elem1 :: restFS ->
            printf "%s \n" (getName elem1)
            match elem1 with
            | File f -> 1 + fileCount restFS
            | Dir (name,f) ->
                printfn "%s \n" name
                fileCount f + fileCount restFS
    fileCount fs   

*)

let rec count (fs :FileSystem) :int= 
    match fs with
    | [] -> 0
    | file :: system -> 
        match getElement file with
        | File f -> 1 + count system
        | Dir (name, f) -> count f + count system

let t1 = [ (Dir ("d1", [ (Dir ("D2",[(File "f1", ReadWrite) ]), ReadWrite) ]), ReadWrite); 
            (Dir ("d4",[(Dir ("d1",[(Dir ("d1",[(File "f1", ReadWrite)]), ReadWrite)]), ReadWrite)]), ReadWrite); (File "f1", ReadWrite)];;        

// 4. Define a function
// changePermissions Permission -> string list -> FileSystem -> FileSystem
// that will apply the specified permission the file or directory
// represented by a string list. For example, list ["Dir1";"Dir2";"File1"]
// represents a structure where "Dir1" is in the root directory, "Dir2" is
// in "Dir1" and "File1" is in "Dir2".

let rec changePermissions (perm :Permission) (path :string list) (fs :FileSystem) :FileSystem =
    match path with
    | [fileName] ->
        //printf "%s \n" fileName 
        match fs with 
        | elem :: system when (getName (getElement elem)) = fileName ->
            match getElement elem with
            | File file -> (File file, perm) :: system
            | Dir (name, sys) -> (Dir (name, sys), perm) :: system
        | elem :: system -> elem :: changePermissions perm [fileName] system
        | _ -> fs    
    | fileName :: restOfPath ->
        printf "%s \n" fileName
        match fs with
        | elem :: system when (getName (getElement elem)) = fileName ->
            printf "%s \n" "equal"
            match getElement elem with
            | Dir (nameOfDir, content) -> 
                printf "%s \n" "Going deeper"
                changePermissions perm restOfPath content
            | _ -> fs
        | elem :: system -> 
            printf "%s \n" "Not equal"
            changePermissions perm path system           
    | _ -> fs    

// 5. Modify the implementations of createFile and createDir to honor the
// permissions of the current file system level, i.e. if the permission 
// of the current directory is not Write or ReadWrite, the function should fail
// with an exception and an appropriate message (that you should formulate yourself).
// Hint: use the built-in failwith function.

// 6. Implement the function
// locate : string -> FileSystem -> string list list
// that will locate all files and directories with name matching the first argument
// of the function. The return value should be a list of paths to the files and
// directories. Each path is a list of strings indicating the parent directory
// structure.
// Note that the locate should honor the permissions, i.e. the files from
// directories without a read permission should not be returned.

// 7. Implement the function
// delete : string list -> FileSystem -> FileSystem
// which will delete the file or directory specified with the first argument (the path
// represented by the string list) from a file system and return a file system
// not containing the file or directory represented by the first argument.
// If the file or directory does not have a write permission, the deletion should not
// be performed and the original file system should be returned.
 
// Bonus (1p):
// 8. Implement the function:
// recursiveDelete : string list -> FileSystem ->FileSystem
// that will delete a file or directory given as the first argument from a file
// system specified as the second argument.
// In case the item to be deleted is a directory, it needs to honor permissions
// and recursively only delete files with write permissions from directories with 
// write permissions. Subdirectories which will become empty need to be deleted as well. 
