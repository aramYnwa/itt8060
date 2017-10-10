(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: Discriminated unions, higher order functions

  ------------------------------------
  Name: Aramais Khachatryan 
  TUT Student ID: arkhac@ttu.ee
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060 under your name, into a file coursework3/coursework3.fsx by October 11, 2017.
  NB! The deadline has been extended!
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// 1. Create a type BibliographyItem data structure based on discriminated unions such that it supports data
// for bibliography items for
// * article (journal paper)
// * inproceedings (conference paper)
// * book
// * MSc thesis
// * Web page (misc)
// Use the specifications given at http://newton.ex.ac.uk/tex/pack/bibtex/btxdoc/node6.html
// You should support all mandatory and optional fields of each entry. The names of the fields should
// be the same as in the referenced web page. You should capitalize the names of constructors in 
// discriminated unions.


type ArticleData = {
    author  :string list
    title   :string
    journal :string
    year    :int
    volume  :int option
    number  :int option
    pages   :(int*int) option
    month   :string option
    note    :string option
}

type InproceedingsData = {
    author          :string list
    title           :string
    booktitle       :string
    year            :int
    editor          :string option
    number          :int option
    volume          :int option
    series          :string option
    pages           :(int*int) option
    address         :string option
    month           :string option
    organization    :string option
    publisher       :string option
    note            :string option
}

type BookData = {
    author      :string list
    editor      :string list option
    title       :string
    publisher   :string
    year        :int
    volume      :int option
    number      :int option
    series      :string option
    address     :string option
    edition     :string option
    month       :string option
    note        :string option
}

type MscThesisData = {
    author      :string list
    title       :string
    school      :string
    year        :int
    type_       :string option
    address     :string option
    month       :string option
    note        :string option
}

type MiscData = {
    author          :string list option
    title           :string option
    howpublished    :string option
    month           :string option
    year            :int option
    note            :string option
}

type BibliographyItem = 
  | Article of ArticleData
  | Inproceedings of InproceedingsData
  | Book of BookData
  | MscThesis of MscThesisData
  | Misc of MiscData


//We will define some functions to get different attributes from different type of data

//Temp function to conver string option list to string list
  
let getAuthor (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> item.author
  | Book item -> item.author
  | Inproceedings item -> item.author
  | MscThesis item -> item.author
  | Misc item -> if item.author.IsSome then item.author.Value else [""]

let rec formatAuthor (item :string list) = 
  match item with
  | head :: head1 :: tail when tail.IsEmpty  -> head.ToString() + " and " + head1.ToString()
  | head :: tail when tail.IsEmpty -> head.ToString()
  | head :: tail -> head.ToString() + ", " + formatAuthor tail
  | _ -> ""

let getTitle (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> item.title.ToString() 
  | Book item -> item.title.ToString() 
  | Inproceedings item -> item.title.ToString() 
  | MscThesis item -> item.title.ToString() 
  | Misc item -> if item.title.IsSome then item.title.Value.ToString() else ""

let getJournal (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> item.journal.ToString()
  | _ -> ""

let getYear (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> item.year.ToString()
  | Book item -> item.year.ToString() 
  | Inproceedings item -> item.year.ToString() 
  | MscThesis item -> item.year.ToString() 
  | Misc item -> if item.year.IsSome then item.year.Value.ToString() else ""

let getVolume (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> if item.volume.IsSome then item.volume.Value.ToString() else ""
  | Book item -> if item.volume.IsSome then item.volume.Value.ToString() else ""
  | Inproceedings item -> if item.volume.IsSome then item.volume.Value.ToString() else ""
  | _ -> ""

let getNumber (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> if item.number.IsSome then item.number.Value.ToString() else ""
  | Inproceedings item -> if item.number.IsSome then item.number.Value.ToString() else ""
  | _ -> ""

let getPages (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> if item.pages.IsSome then item.pages.Value else (-1, -1)
  | Inproceedings item -> if item.pages.IsSome then item.pages.Value else (-1, -1)
  | _ -> (-1, -1)

let formatPages (tuple: int*int) =
  match tuple with
  | (-1, 1) -> ""
  | (a,b) -> a.ToString() + "-" + b.ToString()

let getMonth (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> if item.month.IsSome then item.month.Value.ToString() + " " else ""
  | Book item -> if item.month.IsSome then item.month.Value.ToString() + " " else ""
  | Inproceedings item -> if item.month.IsSome then item.month.Value.ToString() + " " else ""
  | MscThesis item -> if item.month.IsSome then item.month.Value.ToString() + " " else ""
  | Misc item -> if item.month.IsSome then item.month.Value.ToString() + " " else ""

let getNote (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> if item.note.IsSome then item.note.Value.ToString() else ""
  | Book item -> if item.note.IsSome then item.note.Value.ToString() else ""
  | Inproceedings item -> if item.note.IsSome then item.note.Value.ToString() else ""
  | MscThesis item -> if item.note.IsSome then item.note.Value.ToString() else ""
  | Misc item -> if item.note.IsSome then item.note.Value.ToString() else ""

let getBookTitle (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Inproceedings item -> item.booktitle.ToString()
  | _ -> ""

let getSeries (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Book item -> if item.series.IsSome then item.series.Value.ToString() else ""
  | Inproceedings item -> if item.series.IsSome then item.series.Value.ToString() else ""
  | _ -> ""

let getEditor (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Inproceedings item -> if item.editor.IsSome then item.editor.Value.ToString() else ""
  | _ -> ""

let getAddres (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Book item -> if item.address.IsSome then item.address.Value.ToString() else ""
  | Inproceedings item -> if item.address.IsSome then item.address.Value.ToString() else ""
  | MscThesis item -> if item.address.IsSome then item.address.Value.ToString() else ""
  | _ -> ""

let getOrganization (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Inproceedings item ->  if item.organization.IsSome then item.organization.ToString() else ""
  | _ -> ""          

let getPublisher (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Book item -> item.publisher
  | Inproceedings item -> if item.publisher.IsSome then item.publisher.Value else ""
  | _ -> ""

let getEdition (bibliographyItem :BibliographyItem) =
  match bibliographyItem with
  | Book item -> if item.edition.IsNone then item.edition.Value else ""
  | _ -> ""

let getSchool (bibliographyItem :BibliographyItem) =
  match bibliographyItem with
  | MscThesis item ->  item.school
  | _ -> ""

let getType (bibliographyItem :BibliographyItem) =
  match bibliographyItem with
  | MscThesis item -> if item.type_.IsSome then item.type_.Value else ""
  | _ -> ""

let getHowPublished (bibliographyItem :BibliographyItem) =
  match bibliographyItem with
  | Misc item -> if item.howpublished.IsSome then item.howpublished.Value else ""
  | _ -> ""

// 2. Create a value bibliographyData : BibliographyItem list that contains
// at least 10 different publications on your favourite topic from http://dblp.uni-trier.de/ 
// and the MSc thesis databases of UT and TTÜ. At least one instance of every publication needs to be 
// Please note that you need not read the papers, just pick 10 papers that sound interesting to you from the database.

let article1 = Article {
    author  = ["Jim X. Chen"];
    title   = "The Evolution of Computing: AlphaGo";
    journal = "Computing in Science and Engineering";
    year    = 2016;
    volume  = Some 18;
    number  = Some 4;
    pages   = Some (4,7);
    month   = Some "May";
    note    = None }

let article2 = Article {
    author  = ["Jim X. Chen"];
    title   = "The Evolution of Computing: AlphaGo";
    journal = "Computing in Science and Engineering";
    year    = 2016;
    volume  = Some 18;
    number  = Some 4;
    pages   = Some (4,7);
    month   = None
    note    = None }
let inproceedings1 = Inproceedings {
    author          = ["Dmitry Plotnikov"; "Dmitry Melnik"; "Mamikon Vardanyan"; "Ruben Buchatskiy"; "Roman Zhuykov"; "JeHyung Lee"];
    title           = "Automatic Tuning of Compiler Optimizations and Analysis of their Impact";
    booktitle       = "Proceedings of the International Conference on Computational Science";
    year            = 2013;
    editor          = Some "Vassil N. Alexandrov, Michael Lees, Valeria V. Krzhizhanovskaya, Jack J. Dongarra, Peter M. A. Sloot"
    volume          = Some 18
    series          = Some"Procedia Computer Science"
    pages           = Some (1312, 1321)
    address         = Some "Barcelona, Spain"
    month           = Some "June"
    organization    = None
    publisher       = Some "Elsevier"
    note            = None
    number          = None
}

let book1 = Book {
    author      = ["Steve McConnell"];
    title       = "Code complete - a practical handbook of software construction, 2nd Edition";
    publisher   = "Microsoft Press";
    year        = 2004;
    volume      = None
    series      = None
    address     = None
    edition     = None
    month       = None
    note        = None
    editor      = None
    number      = None
}

let misc1 = Misc {
  author        = None
  title         = None
  howpublished  = None
  month         = None
  year          = None
  note          = None
}

// 3. Create a function formatInACMReferenceStyle : BibliographyItem -> string that will format the bibliography items
// using the reference style specified here: http://www.acm.org/publications/authors/reference-formatting


let formatInACMReferenceStyle (item :BibliographyItem) =
  match item with
  | Article it -> sprintf "%s. %s. %s %s %s, %s (%s%s) %s."  (formatAuthor (getAuthor item)) 
                                                        (getYear    item) 
                                                        (getTitle   item) 
                                                        (getJournal item) 
                                                        (getVolume  item) 
                                                        (getNumber  item)
                                                        (getMonth   item)
                                                        (getYear    item)
                                                        (formatPages (getPages item))
                                                      
formatInACMReferenceStyle article1

// 4. Write a function compareByAuthorYear : BibliographyItem -> BibliographyItem -> int that will compare the authors and year of the
// bibliography item in the same way as specified in coursework2.
let rec compareLists (firstList: string list) (secondList: string list) =
    match firstList, secondList with 
    | [], [] -> 0
    | [], _ -> 1
    | _, [] -> -1
    | h1::t1, h2::t2 when System.String.Compare(h1,h2) = 0 -> compareLists t1 t2
    | h1::t1, h2::t2 -> System.String.Compare(h1,h2) 
let compareByAuthorYear (item1 :BibliographyItem) (item2 :BibliographyItem) = 
  match (item1, item2) with
  | (item1, item2) when compareLists (getAuthor item1) (getAuthor item2) <> 0 -> compareLists (getAuthor item1) (getAuthor item2)
  | (item1, item2) when getYear item1 > getYear item2 -> 1
  | (item1, item2) when getYear item1 < getYear item2 -> -1
  | _ -> 0

// 5. Write a function orderBibliography: (BibliographyItem -> BibliographyItem -> int) -> BibliographyItem list -> BibliographyItem list
// That will order the list of bibliography items according to the given comparison function. 

let orderBibliography (func : BibliographyItem -> BibliographyItem -> int) (l :BibliographyItem list) =
  l |> List.sortWith(func)

// 6. Write a function formatBibliographyItems : (BibliographyItem -> string) -> BibliographyItem list -> string list that will take
// a formatting function and a bibliography list and produce a string list that contains formatted bibliography.

// 7. Write a function getNumberedBibliography : BibliographyItem list -> string
// that contains a numbered bibliography where each bibliography item is preceded with a sequence number surrounded
// by square brackets [] and ends with a newline character '\n'.
// The implementation should involve List.fold or List.foldBack function, whichever you deem appropriate.

// 8. Create 5 appropriate functions to create BibliographyItem data instances. Please note that 
// it is up to you to define the internal data structure. The following functions will be used for generating data in your
// format.
(* 
createArticle :
  author:string list ->
    title:string ->
      journal:string ->
        year:int ->
          volume:int option ->
            number:int option ->
              (int * int) option ->
                month:int option ->
                  note:string option -> BibliographyItem
*)
(*
createBook :
  author:string option list ->
    editor:string option list ->
      title:string ->
        publisher:string ->
          year:int ->
            volume:int option ->
              number:int option ->
                series:int option ->
                  address:string option ->
                    edition:string option ->
                      month:int option ->
                        note:string option -> BibliographyItem
*)
(*
createInProceedings :
  author:string list->
    title:string ->
      booktitle:string ->
        year:int ->
          editor:string option list ->
            volume:int option ->
              number:int option ->
                series:string option ->
                  (int * int) option ->
                    address:string option ->
                      month:int option ->
                        organization:string option ->
                          publisher:string option ->
                            note:string option -> BibliographyItem
*)
(*
createMScThesis :
  author:string list ->
    title:string ->
      school:string ->
        year:string ->
          type_:string option ->
            address:string option ->
              month:int option ->
                note:string option -> BibliographyItem

*)
(*
createMisc :
  author:string option list ->
    title:string option ->
      howpublished:string option ->
        month:int option ->
          year:int option ->
            note:string option -> BibliographyItem

*)