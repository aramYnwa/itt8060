(*

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
    [Dir("~/",[]), ReadWrite]

//Our empty file system has this structure
// [(Dir ("~/",[]), ReadWrite)]

let getPerm (tpl : Element * Permission) = 
    match tpl with
    | (el :Element, p :Permission) -> p

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

// 1. Define two functions 
// createFile : string -> FileSystem -> FileSystem
// that will create a file into the root directory of the 
// current file system.
// The first argument is the file name, the second
// is the filesystem to create the file into. 
// (Permissions are initially assumed to be ReadWrite, check task 5)  

let createFile (file :string) (filesystem :FileSystem) :FileSystem =
    let newFile = File file
    let root = getElement filesystem.Head
    [Dir ((getName root), ((newFile, ReadWrite) :: (getDirContent root))), ReadWrite]

// createDir : string -> FileSystem -> FileSystem
// that will create a directory into the root directory of the current
// file system.
// The second argument is the name of the directory 
// (Permissions are initially assumed to be ReadWrite, check task 5)  


let createDir (dirName :string) (filesystem :FileSystem) :FileSystem=
    let newDir = Dir (dirName, [])
    let root = getElement filesystem.Head
    [Dir ((getName root), ((newDir, ReadWrite) :: (getDirContent root))), ReadWrite]

// 2. Define a function 
// createSubDir : string -> FileSystem -> FileSystem -> FileSystem
// that will create a directory with name given in the first argument and
// contents given as the second argument into a file system given
// as the third argument.


let createSubDir (dirName :string) (fs1 :FileSystem) (fs2 :FileSystem) :FileSystem =
    let newFS = Dir (dirName, getDirContent (getElement fs1.Head))
    let root = getElement fs2.Head
    [Dir((getName root), ((newFS, ReadWrite) :: (getDirContent root))), ReadWrite]
  
// 3. Define a function
// count : FileSystem -> int
// that will recursively count the number of files in the current filesystem.

let count (fs :FileSystem) = 
    let root = fs.Head
    let FSContent = getDirContent (getElement root)
    let rec fileCount (rootFS :FileSystem) =
        match rootFS with
        | [] -> 0
        | elem1 :: restFS ->
            match getElement elem1 with
            | File f -> 1 + fileCount restFS
            | Dir (name,f) -> fileCount f + fileCount restFS
    fileCount fs   


//Some examples for testing
let t1 = [Dir ("~/", 
                [ (Dir 
                    ("d1", 
                        [ (Dir 
                            ("D2", [(File "f1", Write) ]), 
                          ReadWrite) 
                        ]), 
                  Write); 
                  (Dir 
                    ("d4",
                        [(Dir 
                            ("d1",
                                [(Dir 
                                    ("d1", [(File "f1", ReadWrite)]), 
                                ReadWrite)]), 
                     Read)]), 
                  ReadWrite); 
                  (File "f1", Write)]), 
           ReadWrite];;        

// 4. Define a function
// changePermissions Permission -> string list -> FileSystem -> FileSystem
// that will apply the specified permission the file or directory
// represented by a string list. For example, list ["Dir1";"Dir2";"File1"]
// represents a structure where "Dir1" is in the root directory, "Dir2" is
// in "Dir1" and "File1" is in "Dir2".

let rec changePermission (perm :Permission) (path :string list) (fs :FileSystem) :FileSystem =
        match path with
        | [fileName] ->
            match fs with 
            | elem :: system when (getName (getElement elem)) = fileName ->
                match getElement elem with
                | File file -> (File file, perm) :: system
                | Dir (name, sys) -> (Dir (name, sys), perm) :: system
            | elem :: system -> elem :: changePermission perm [fileName] system
            | _ -> fs    
        | fileName :: restOfPath ->
            match fs with
            | elem :: system when (getName (getElement elem)) = fileName ->
                match getElement elem with
                | Dir (nameOfDir, content) -> 
                    let newDir = changePermission perm restOfPath content
                    let p = getPerm elem
                    (Dir(nameOfDir, newDir), p) :: system
                | _ -> fs
            | elem :: system -> elem :: changePermission perm path system
            | _ -> fs           
        | _ -> fs 
   
       
changePermission Read ["~/"; "d4"; "d1"] t1;;

// 5. Modify the implementations of createFile and createDir to honor the
// permissions of the current file system level, i.e. if the permission 
// of the current directory is not Write or ReadWrite, the function should fail
// with an exception and an appropriate message (that you should formulate yourself).
// Hint: use the built-in failwith function.

let createFilePerm (file :string) (perm :Permission) (fs :FileSystem) = 
    let root = getElement fs.Head
    let rootPerm = getPerm fs.Head
    match rootPerm with
    | Write | ReadWrite ->
        let newFile = File file
        [Dir ((getName root), ((newFile, perm) :: (getDirContent root))), rootPerm]
    | _ -> failwith("Access denied !!! No privilege to write in root.")    


let createDirPerm (dirName :string) (perm :Permission) (fs :FileSystem) = 
    let root = getElement fs.Head
    let rootPerm = getPerm fs.Head
    match rootPerm with
    | Write | ReadWrite ->
        let newDir = Dir (dirName, [])
        [Dir ((getName root), ((newDir, perm) :: (getDirContent root))), rootPerm]
    | _ -> failwith("Access denied !!! No privilege to write in root.")

// 6. Implement the function
// locate : string -> FileSystem -> string list list
// that will locate all files and directories with name matching the first argument
// of the function. The return value should be a list of paths to the files and
// directories. Each path is a list of strings indicating the parent directory
// structure.
// Note that the locate should honor the permissions, i.e. the files from
// directories without a read permission should not be returned.

let locate (name :string) (fs :FileSystem) :string list list = 
    let rec loc (name :string) (fs :FileSystem) (path :string list) (acc :string list list) =
        match fs with
        | [] -> acc
        | file :: system -> 
            let perm = getPerm file
            let fl = getElement file
            match perm with
            | Write -> 
                //failwith("NoAccess")
                loc name system path acc
            | _ when getName fl = name ->
                match fl with 
                | File tempFile ->
                    printf "ayo! \n"
                    let acc = (path @ [name]) :: acc
                    loc name system path acc
                | Dir (_,tempDir) ->
                    let newPAth = path @ [name]
                    let result = loc name tempDir newPAth (newPAth :: acc)    
                    loc name system path result
            | _ -> 
                match fl with
                | File _ -> loc name system path acc
                | Dir (nameDir, tempDir) ->
                    let newPath = path @ [nameDir]
                    let result = loc name tempDir newPath acc
                    loc name system path result
        | _ -> acc  
    let acc :string list list = []
    let rootElem = getElement fs.Head
    let perm = getPerm fs.Head
    let rootName = getName rootElem
    let rootContent = getDirContent (rootElem)
    match perm with
    | Write -> failwith("No!")
    | _ when rootName = name -> 
        let acc = [rootName] :: acc
        loc name rootContent [rootName] acc 
    | _ -> loc name rootContent [rootName] acc       


locate "f1" t1

// 7. Implement the function
// delete : string list -> FileSystem -> FileSystem
// which will delete the file or directory specified with the first argument (the path
// represented by the string list) from a file system and return a file system
// not containing the file or directory represented by the first argument.
// If the file or directory does not have a write permission, the deletion should not
// be performed and the original file system should be returned.

let rec delete (path :string list) (fs :FileSystem) :FileSystem =
    match path with
    | [] -> fs
    | [root] ->
        match fs with
        | [] -> []
        | f1 :: system when getName (getElement f1) = root ->
            let perm = getPerm f1
            match perm with 
            | Read -> fs
            | _ -> system
        | f1 :: system ->
            f1 :: delete path system
    | path1 :: restPath ->
        match fs with 
        | [] -> []
        | f1 :: system when getName (getElement f1) = path1 ->
            let perm  = getPerm f1
            match perm with
            | Read -> f1 :: system
            | _ ->
                match getElement f1 with 
                | File f -> failwith ("Wrong Path!")
                | Dir (_,_) ->
                    let res = delete restPath (getDirContent (getElement f1))
                    (Dir (path1, res), perm) :: system
        | f1 :: system -> f1 :: delete path system          

// Bonus (1p):
// 8. Implement the function:
// recursiveDelete : string list -> FileSystem ->FileSystem
// that will delete a file or directory given as the first argument from a file
// system specified as the second argument.
// In case the item to be deleted is a directory, it needs to honor permissions
// and recursively only delete files with write permissions from directories with 
// write permissions. Subdirectories which will become empty need to be deleted as well. 

// If we delete filesystem recursively we will get empty file system.
// To delete root as well, we need to do that explicitly.
let rec del (fs:FileSystem) :FileSystem =
    match fs with
    | [] -> []
    | file :: system ->
        let perm = getPerm file
        match getElement file with 
        | File fl when perm <> Read -> del system
        | File fl -> file :: (del system)
        | Dir (dirName, content) when perm <> Read ->
            let newDir = del content
            if newDir.IsEmpty then (del system) else
                (Dir(dirName, newDir), perm) :: (del system)
        | Dir (dirName, content) -> file :: (del system)   

let rec recursiveDelete (path :string list) (fs :FileSystem) :FileSystem = 
    match path with
    | [] -> fs
    | [root] -> 
        match fs with
        | [] -> []
        | head :: tail when getName (getElement head) = root ->
            let perm = getPerm head
            match perm with
            | Write -> head :: tail
            | _ -> 
                match getElement head with
                | File file -> tail
                | Dir (name, content) -> (del [head]) @ tail
        | head :: tail -> head :: recursiveDelete path tail
    | hPath :: tPath ->
        match fs with
        | [] -> []
        | head :: tail when getName (getElement head) = hPath ->
            let perm = getPerm head
            match perm with
            | Write -> head ::tail
            | _ ->
                match getElement head with
                | File _ -> failwith "Wrong PATH!"
                | Dir (dirName, content) -> [(Dir (dirName, recursiveDelete tPath content), getPerm head)] @ tail
        | head :: tail -> head :: recursiveDelete path tail     
        
