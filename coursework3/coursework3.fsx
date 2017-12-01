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
    editor          :string option list
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
    editor      :string option list
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
    year        :string
    type_       :string option
    address     :string option
    month       :string option
    note        :string option
}

type MiscData = {
    author          :string option list
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
let rec optionListToList (l: string option list) :string list =
  match l with
  | [] -> []
  | head :: tail when head.IsNone -> []
  | head :: tail -> head.Value :: optionListToList tail

let getAuthor (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> item.author
  | Book item -> item.author
  | Inproceedings item -> item.author
  | MscThesis item -> item.author
  | Misc item -> optionListToList item.author

let rec formatAuthor (item :string list) = 
  match item with
  | head :: head1 :: tail when tail.IsEmpty  -> head.ToString() + " and " + head1.ToString() + " "
  | head :: tail when tail.IsEmpty -> head.ToString() + ". "
  | head :: tail -> head.ToString() + ", " + formatAuthor tail
  | _ -> ""

let getTitle (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> item.title.ToString() 
  | Book item -> item.title.ToString() 
  | Inproceedings item -> item.title.ToString() + "."
  | MscThesis item -> item.title.ToString() + "."
  | Misc item -> if item.title.IsSome then item.title.Value.ToString() else ""

let getJournal (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> item.journal.ToString()
  | _ -> ""

let getYear (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> item.year.ToString() + ". "
  | Book item -> item.year.ToString() + ". "
  | Inproceedings item -> item.year.ToString()  + ". "
  | MscThesis item -> item.year + ". "
  | Misc item -> if item.year.IsSome then item.year.Value.ToString() + "." else ""

let getVolume (bibliographyItem :BibliographyItem) =
  match bibliographyItem with 
  | Article item -> if item.volume.IsSome then item.volume.Value.ToString() else ""
  | Book item -> if item.volume.IsSome then "Vol. " + item.volume.Value.ToString() else ""
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
  | Article item -> if item.month.IsSome then item.month.Value.ToString() else ""
  | Book item -> if item.month.IsSome then item.month.Value.ToString() else ""
  | Inproceedings item -> if item.month.IsSome then item.month.Value.ToString() else ""
  | MscThesis item -> if item.month.IsSome then item.month.Value.ToString() else ""
  | Misc item -> if item.month.IsSome then item.month.Value.ToString() else ""

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
  | Inproceedings item -> if item.series.IsSome then "(" + item.series.Value.ToString() + ")." else ""
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
  | Inproceedings item -> if item.publisher.IsSome then item.publisher.Value + " Press," else ""
  | _ -> ""

let getEdition (bibliographyItem :BibliographyItem) =
  match bibliographyItem with
  | Book item -> if item.edition.IsNone then "( " +  item.edition.Value + " ed.)" else ""
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

let months = Map.empty.Add(1, "January")
                      .Add(2, "February")
                      .Add(3, "March")
                      .Add(4, "April")
                      .Add(5, "May")
                      .Add(6, "June")
                      .Add(7, "July")
                      .Add(8, "August")
                      .Add(9, "September")
                      .Add(10, "October")
                      .Add(11, "November")
                      .Add(12, "December")



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
    month   = None;
    note    = None }

let article2 = Article {
    author  = ["Jian-min Liu, Min-hua Yang"];
    title   = "Deep Learning-Based Classification of Remote Sensing Image";
    journal = "JCP";
    year    = 2018;
    volume  = Some 13;
    number  = Some 1;
    pages   = Some (44,48);
    month   = None;
    note    = None }
let inproceedings1 = Inproceedings {
    author          = ["Dmitry Plotnikov"; "Dmitry Melnik"; "Mamikon Vardanyan"; "Ruben Buchatskiy"; "Roman Zhuykov"; "JeHyung Lee"];
    title           = "Automatic Tuning of Compiler Optimizations and Analysis of their Impact";
    booktitle       = "Proceedings of the International Conference on Computational Science";
    year            = 2013;
    editor          = [Some "Vassil N. Alexandrov, Michael Lees, Valeria V. Krzhizhanovskaya, Jack J. Dongarra, Peter M. A. Sloot"]
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
let inproceedings2 = Inproceedings {
    author          = ["Michael C. Fu"];
    title           = "AlphaGo and Monte Carlo tree search: The simulation optimization perspective";
    booktitle       = "Winter Simulation Conference";
    year            = 2016;
    editor          = [None]
    volume          = None
    series          = None
    pages           = Some (659,670)
    address         = Some "Washington, DC, USA"
    month           = Some "December"
    organization    = None
    publisher       = Some "IEEE"
    note            = None
    number          = None
}

let book1 = Book {
    author    = ["Mark Hoogendoorn and Burkhardt Funk"];
    title     = "Machine Learning for the Quantified Self - On the Art of Learning from Sensory Data";
    year      = 2018;
    publisher = "Springer";
    volume    = Some 35;
    series    = Some "Cognitive Systems Monographs";
    number    = None;
    address   = None;
    edition   = None;
    month     = None;
    note      = None;
    editor    = [None]
}

let book2 = Book {
    author    = ["Steve McConnell"];
    title     = "Code complete - a practical handbook of software construction";
    year      = 2004;
    publisher = "Microsoft press";
    volume    = None;
    series    = None;
    number    = None;
    address   = None;
    edition   = Some "2nd";
    month     = None;
    note      = None;
    editor    = [None]
}

let mscThesis1 = MscThesis {
    author      = ["Vostan Azatyan"];
    title       = "On the transformation of Petri nets into BPMN models";
    school      = "UT";
    year        = "2017";
    type_       = None;
    address     = None;
    month       = None;
    note        = None
}

let mscThesis2 = MscThesis {
    author      = ["Ishikyan Tatevik"];
    title       = "Presenting Business Process Improvement Changes – A Systematic Literature Review";
    school      = "UT";
    year        = "2017";
    type_       = None;
    address     = None;
    month       = None;
    note        = None
}
let misc1 = Misc {
  author        = [Some "Wikipedia"]
  title         = Some "WikipediA: the Free Encyclopedia"
  howpublished  = None
  month         = None
  year          = Some 2017
  note          = None
}

let misc2 = Misc {
  author        = [Some "Harry Thornburg"]
  title         = Some "Introduction to Bayesian Statistics"
  howpublished  = None
  month         = Some "March"
  year          = Some 2001
  note          = None
}
let bibliographyData = [article1; article2; inproceedings1; inproceedings2; book1; book2; mscThesis1; mscThesis2; misc1; misc2]
// 3. Create a function formatInACMReferenceStyle : BibliographyItem -> string that will format the bibliography items
// using the reference style specified here: http://www.acm.org/publications/authors/reference-formatting

let t (item :BibliographyItem) :string =
  match item with
  | Book it -> "Book"
  | Inproceedings it -> "Inprocessing"
  | Article it -> "Article"

let formatInACMReferenceStyle (item :BibliographyItem) :string =
  match item with
  | Article       it -> (formatAuthor (getAuthor item)) +  (getYear item) + (getTitle item) 
                        + (getJournal item) + (getVolume item) + (getNumber item) + (getMonth item) + (getYear item) + (formatPages (getPages item))
                       
  | Inproceedings it -> (formatAuthor (getAuthor item)) +  (getYear item) + (getTitle item) + " In " 
                        + (getBookTitle item) + " " + (getSeries item) + " " + (getPublisher item) + " " + (getAddres item) + " " + (formatPages (getPages item))
                        
  | MscThesis     it -> (formatAuthor (getAuthor item)) + (getYear item) + (getTitle item) + " Master's thesis. " + (getSchool item)
                        
  | Misc          it -> (formatAuthor (getAuthor item)) + (getYear item) + (getTitle item) + " " + (getHowPublished item) + " " + (getMonth item) + " " + (getNote item)
  
  | Book it when getVolume item <> "" -> (formatAuthor (getAuthor item)) + (getYear item) + (getTitle item) + ", " + (getVolume item) + " " + (getPublisher item)
  
  | Book it when getSeries item <> "" -> (formatAuthor (getAuthor item)) + (getYear item) + (getTitle item) + " " + (getEdition item) + " The name of the series " +
                                          (getSeries item) + " " + (getPublisher item) + " " + (getAddres item)
  | Book                          it  -> (formatAuthor (getAuthor item)) + (getYear item) + (getTitle item) + " " + (getEdition item) + " " + (getAddres item)


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

let formatBibliographyItems (f : BibliographyItem -> string) (l :BibliographyItem list) :string list =
  l |> List.map(f)

formatBibliographyItems formatInACMReferenceStyle bibliographyData

// 7. Write a function getNumberedBibliography : BibliographyItem list -> string
// that contains a numbered bibliography where each bibliography item is preceded with a sequence number surrounded
// by square brackets [] and ends with a newline character '\n'.
// The implementation should involve List.fold or List.foldBack function, whichever you deem appropriate.

let getNumberedBibliography1 (l :BibliographyItem list )  :string =
  let temp = formatBibliographyItems formatInACMReferenceStyle l
  let rec concatListWithIndex (l :string list) (acc :int) :string =
    match l with
    | [] -> ""
    | [head] -> head + " [" + acc.ToString() + "] \n"
    | head :: tail -> head + " [" + acc.ToString() + "] \n" + concatListWithIndex tail (acc + 1)
  concatListWithIndex temp 1


let getNumberedBibliography (l :BibliographyItem list)  =
 let temp = formatBibliographyItems formatInACMReferenceStyle l
 List.fold2(fun (acc: string) x y -> acc + "[" + x.ToString() + "]" + y ) "" [1..temp.Length] temp 

formatBibliographyItems formatInACMReferenceStyle bibliographyData
getNumberedBibliography bibliographyData 


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

let createArticle (author_ :string list) (title_ :string list) (journal_ :string) (year_ :int) 
                  (volume_ :int option) (number_ :int option) (pages_ :(int*int) option) (month_ :int option) (note_ :string option) :BibliographyItem = 
                  let article = Article {
                      author    = author_
                      title     = List.fold(fun acc n -> acc + n) " " title_
                      journal   = journal_
                      year      = year_
                      volume    = volume_
                      number    = number_
                      pages     = pages_
                      month     = if month_.IsSome then Some (Map.find (month_.Value) months) else None
                      note      = note_
                  }
                  article

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

let createBook (author_ :string option list) (editor_ :string option list) (title_ :string) (publisher_ :string) (year_ :int) (volume_ :int option) 
               (number_ :int option) (series_ :int option) (address_ :string option) (edition_ :string option) (month_ :int option) (note_ :string option) :BibliographyItem = 
                  let book = Book {
                      author    = optionListToList author_
                      editor    = editor_
                      title     = title_
                      publisher = publisher_
                      year      = year_
                      volume    = volume_
                      number    = number_
                      series    = if series_.IsSome then Some (series_.Value.ToString()) else None
                      address   = address_
                      edition   = edition_
                      month     = if month_.IsSome then Some (Map.find (month_.Value) months) else None
                      note      = note_
                  }
                  book
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

let createInProceedings (author_ :string list) (title_ :string) (booktitle_ :string) (year_ :int) (editor_ : string option list) 
                        (volume_ :int option) (number_ :int option) (series_ :int option) (pages_ :(int*int) option) (address_ :string option) 
                        (month_ :int option) (organization_ :string option) (publisher_ :string option) (note_ :string option) :BibliographyItem = 
                  let inproceedings = Inproceedings {
                      author        = author_
                      title         = title_
                      booktitle     = booktitle_
                      year          = year_
                      editor        = editor_
                      volume        = volume_
                      number        = number_
                      series        = if series_.IsSome then Some (series_.Value.ToString()) else None
                      pages         = pages_
                      address       = address_
                      month         = if month_.IsSome then Some (Map.find (month_.Value) months) else None
                      organization  = organization_
                      publisher     = publisher_
                      note          = note_
                  }
                  inproceedings
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
let createMScThesis (author_ :string list) (title_ :string) (school_ :string) (year_ :string) 
                    (type__ :string option) (address_ :string option) (month_ :int option) (note_ :string option) :BibliographyItem = 
                  let mscThesis = MscThesis {
                      author    = author_
                      title     = title_
                      school    = school_
                      year      = year_
                      type_     = type__
                      address   = address_
                      month     = if month_.IsSome then Some (Map.find (month_.Value) months) else None
                      note      = note_
                  }
                  mscThesis
(*
createMisc :
  author:string option list ->
    title:string option ->
      howpublished:string option ->
        month:int option ->
          year:int option ->
            note:string option -> BibliographyItem

*)

let creatMisc (author_ :string option list) (title_ :string option) (howpublished_ :string option)
              (month_ :int option) (year_ :int option) (note_ :string option) = 
              let misc = Misc {
                author        = author_
                title         = title_
                howpublished  = howpublished_
                month         = if month_.IsSome then Some (Map.find (month_.Value) months) else None
                year          = year_
                note          = note_
              }
              misc