(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 9: Asynchronous and reactive programming

  ------------------------------------------------------------------------------
  Name: Aramais Khachatryan
  Student ID: arkhac@ttu.ee
  ------------------------------------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework9.fsx in directory coursework9.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs), if they are required.

  The deadline for completing the above procedure is Friday, December 8, 2017.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)
open System.IO
open System.Net
open System

(*
  Task 1:

  Write a function downloadParallel : (string * string) list -> Async<string []> that takes
  a list of name and URLs pairs and downloads the resources referenced by these URLs in
  parallel. Use the function fetchAsync from the lecture in your
  implementation.
*)

let tprintfn fmt =
    printf "[Thread %d]" System.Threading.Thread.CurrentThread.ManagedThreadId
    printfn fmt

let readToEndAsync (reader : StreamReader) =
    Async.AwaitTask (reader.ReadToEndAsync())

let fetchAsync (nm, url:string) =
   async {
      tprintfn "Creating request for %s..." url
      let request = HttpWebRequest.Create(url)
      let! response = request.AsyncGetResponse()
      use response = response
      tprintfn "Getting response stream for %s..." url
      let stream = response.GetResponseStream()
      tprintfn "Reading response for %s..." url
      use reader = new StreamReader (stream)
      let! text = readToEndAsync reader
      return text
   }

let downloadParallel  (l : (string * string) list) : Async<string []> =
  Async.Parallel [for s1, s2 in l -> fetchAsync(s1, s2)]

let sites = ["fb", "http://www.facebook.com"; "google", "http://www.google.com"]

downloadParallel sites |> Async.Ignore |> Async.RunSynchronously


(*
  Task 2:

  Write a function downloadSemiParallel : (string * string) list -> Async<string []> that
  takes a list of URLs and downloads the resources referenced by these URLs.
  Resources from URLs with the same domain name shall be downloaded
  sequentially, but otherwise, parallelism shall be used. The order of the
  resources in the resulting array can be chosen by you.
*)

 
let downloadSemiParallel (l : (string * string) list)  = 
  let groups = l |> List.groupBy fst |> List.map (fun (f,s) -> s)
  groups |> List.map (fun sites -> 
    downloadParallel sites |> Async.Ignore |> Async.RunSynchronously)
 

let domains = ["fb", "http://www.facebook.com"; "google", "http://www.google.com";
"fb", "http://www.facebook.com/newsfeed"; "google", "http://www.images.google.com"]

downloadSemiParallel domains


(*
  Task 3:

  Write an event stream additions : IObservable<string*string> that emits an event
  everytime a file is created in the current directory. Each such event shall
  carry the name of the created file and the creation time as string tupled together.

  Furthermore, write an event stream removals : IObservable<string*string> that emits
  an event everytime a file is removed from the current directory. Each such
  event shall carry the name of the removed file and the deletion time as string.
*)

let watcher = new FileSystemWatcher (@".")
watcher.EnableRaisingEvents <- true

let created = watcher.Created

let additions = created |> Observable.map (fun (eventArgs : FileSystemEventArgs)  ->
  (sprintf "Created file %s\n " eventArgs.Name,  
    sprintf "Creation time %s\n " (DateTime.Now.ToString()))
)

let reactAddMsg () =  additions|> Observable.add (fun tup -> printf "%s %s" (fst tup) (snd tup))

reactAddMsg()

let removed = watcher.Deleted

let removals = removed |> Observable.map (fun (eventArgs : FileSystemEventArgs)  ->
  (sprintf "Removed file %s\n " eventArgs.Name,  
    sprintf "Deletion time %s\n " (DateTime.Now.ToString()))
)

let reactDelMsg () =  removals|> Observable.add (fun tup -> printf "%s %s" (fst tup) (snd tup))

reactDelMsg()


(*
  Task 4:

  Below you find the definition of a type Change whose values represent changes
  to a directory. Use the event streams from Task 3 to define an event stream
  changes : IObservable<Change> of all file additions and removals in the
  current directory.
*)

type Change =
  | Addition of string*string
  | Removal  of string*string
  


let additionsChange = created |> Observable.map (fun (eventArgs : FileSystemEventArgs)  ->
  Addition(sprintf "Created file %s\n " eventArgs.Name,  
    sprintf "Creation time %s\n " (DateTime.Now.ToString()))
)

let removalsChange = removed |> Observable.map (fun (eventArgs : FileSystemEventArgs)  ->
  Removal (sprintf "Removed file %s\n " eventArgs.Name,  
    sprintf "Deletion time %s\n " (DateTime.Now.ToString()))
)

let changes = Observable.merge additionsChange removalsChange


(*
  Task 5:

  Use the event stream changes from Task 4 to define an event stream
  turnover : IObservable<int> that tells at every file addition or removal how
  much the number of files in this directory has increased since the beginning
  (with negative numbers signifying a decrease). For example, if two files are
  added and one file is removed afterwards, there should be three events, that
  carry the numbers 1, 2, and 1, respectively.
*)

let turnover : IObservable<int> =
   changes |> Observable.scan (
    fun count change -> 
        match change with 
        | Addition (_,_) -> count + 1
        | Removal (_,_)  -> count - 1
    ) 0

let rTurnOver () = turnover |> Observable.add (printfn "%d")

rTurnOver()