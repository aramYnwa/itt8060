open System

(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

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
  repository itt8060 under your name, into a file coursework2/coursework2.fsx by September 29, 2017.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// 1. Create a type BibliographyItem that has the following structure:
// string list * string * int * (int * int)
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents the year of publication
// * The fourth field represents a pair containing the starting page number and ending page number of the paper.

type  BibliographyItem = string list * string * int * (int * int)

// 2. Create a value bibliographyData : BibliographyItem list that contains
// at least 10 different publications on your favourite topic from http://dblp.uni-trier.de/ 
// Please note that you need not read the papers, just pick 10 papers that sound interesting to you from the database.

let BibliographyData : BibliographyItem list = [
    (["Jim X. Chen"],
        "The Evolution of Computing: AlphaGo", 2016, (4,7));
    (["Michael C. Fu"],
        "AlphaGo and Monte Carlo tree search: The simulation optimization perspective", 2016, (659,670));
    (["Jian-min Liu, Min-hua Yang"],
        "Deep Learning-Based Classification of Remote Sensing Image", 2018, (44,48));
    (["Filipe Manuel Clemente"; "Micael Santos Couceiro"], 
        "Practical Implementation of Computational Tactical Metrics for the Football Game", 2017, (1, 9));
    (["Hootan Dehghani"; "Seyed Morteza Babamir"], 
        "A GA based method for search-space reduction of chess game-tree",  2017, (1, 6));
    (["Shelley Burleson"; "Alberto Giordano"], 
        "Extending Metadata Standards for Historical GIS Research: 
        A Case Study of the Holocaust in Budapest and the Armenian Genocide in Turkey",  2015, (1, 22));
    (["Miroslav Kubat"], 
        "An Introduction to Machine Learning, Second Edition.", 2017, (1, 348));
    (["Soujanya Poria"], 
        "Novel symbolic and machine-learning approaches for text-based and multimodal sentiment analysis", 2017, (1,6));
    (["Albrecht Zimmermann"],
        "Basketball predictions in the NCAAB and NBA: Similarities and differences", 2016, (350,364));
    (["Gedas Bertasius, Stella X. Yu, Hyun Soo Park, Jianbo Shi"],
        "Am I a Baller? Basketball Skill Assessment using First-Person Cameras", 2016, (1,5)); ]
   
// 3. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns 
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You are encouraged to use String.Compare to compare individual strings. If the first authors are the same
// then the precedence should be determined by the next author.
// A missing author can be considered to be equivalent to an empty string.
// Please note that your implementation should be recursive over the input lists.

let rec compareLists (firstList: string list) (secondList: string list) =
    match firstList, secondList with 
    | [], [] -> 0
    | [], _ -> 1
    | _, [] -> -1
    | h1::t1, h2::t2 when System.String.Compare(h1,h2) = 0 -> compareLists t1 t2
    | h1::t1, h2::t2 -> System.String.Compare(h1,h2)  
        
// 4. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.

let compareAuthors (article1:BibliographyItem) (article2:BibliographyItem) =
    match (article1, article2) with
    | ((authors1, _, _, _), (authors2, _, _, _)) -> compareLists authors1 authors2

// 5. Make a function
// compareAuthorsYears : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are 
// the same then according to years.

let compareAuthorsYears (item1 :BibliographyItem) (item2 :BibliographyItem) =
    match (item1, item2) with
    | ((authors1, _, year1, _), (authors2, _, year2, _)) when compareLists authors1 authors2 <> 0 ->  compareLists authors1 authors2
    | ((authors1, _, year1, _), (authors2, _, year2, _)) when year1 < year2 -> -1
    | ((authors1, _, year1, _), (authors2, _, year2, _)) when year1 > year2 -> 1
    | ((authors1, _, year1, _), (authors2, _, year2, _)) when year1 = year2 -> 0

// 6. Make a function 
// sortBibliographyByYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the year in ascending order

let sortBibliographyByYear (list :BibliographyItem list) =
    list |> List.sortBy(fun (_, _, year, _) -> year) 

// 7. Make a function 
// sortBibliographyByAuthorYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and year in ascending order

let sortBibliographyByAuthorYear (list: BibliographyItem list) = 
    list |> List.sortWith (fun (item1 :BibliographyItem) (item2 :BibliographyItem) -> compareAuthorsYears item1 item2)

// 8. Make a function
// groupByYear : BibliographyItem list -> BibliographyItem list list
// where the return list contains lists of bibliography items published in the same year.

let groupByYear (list :BibliographyItem list) =
    list
    |> List.groupBy(fun (_ ,_ ,year ,_ ) -> year )
    |> List.map (fun (a,b) -> b)


groupByYear BibliographyData

// 9. Make a function
// commaSeparatedList : BibliographyItem list -> string
// That will return a comma separated string representation of the data.
// Use function composition operator "<<" in your implementation. 

let aggragate :string list list -> string = List.concat >> String.concat ", "

let firstOfTuple  (a,b,c,d) = a
let secondOfTuple (a,b,c,d) = b
let thirdOfTuple  (a,b,c,d) = c
let forthOfTuple  (a,b,c,d) = d

let rec list_ToString list = 
    match list with
    | [] -> ""
    | [h] -> h.ToString()
    | a::b -> a.ToString() + ", " + list_ToString b

let tuple2_ToString (a,b) = a.ToString() + ", " + b.ToString()

let bibiliographyItemToList ((authors, article, year, pages) :BibliographyItem) = 
    [(list_ToString authors).ToString() ; article.ToString(); year.ToString() ; (tuple2_ToString pages).ToString()]
      
bibiliographyItemToList BibliographyData.[1]
let commaSeperatedList (l :BibliographyItem list) =
    let rec merge list = 
        match list with 
        | [] -> []
        | h :: tail -> bibiliographyItemToList h :: merge tail
    aggragate (merge l)
    
commaSeperatedList BibliographyData
    