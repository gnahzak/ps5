(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

Functions for testing and timing the crawler, as well as for
debugging the crawlers returned indexes and pageranks. All
functions to be defined here and called in askshiebs.ml
 *)

open Webtypes ;;
open Pagerank ;;
open Crawler_services ;;
open Crawl ;;
open CS51 ;;

(* tests for the crawler -- series of boolean tests,
   prints to stdout the results of the tests as
   either a failure or success. You should add
   to these tests *)
let crawler_tests () =
  (*
    link_present: a helper function that takes in an index
    returned by your crawler implementation, a string word,
    and a string "link" (path, in this case) and returns
    a boolean (true or false) of
    whether or said link's page contains the word
  *)
  let link_present index word link =
    match LinkIndex.lookup index word with
    | Some v -> LinkSet.member v {host = ""; port = 80; path = link}
    | _ -> false in

  (* set num pages to search to a minimum of 20, to avoid failure in
  cases where num pages is low *)
  let num_pages_to_search = max num_pages_to_search 20 in

  (* test the crawler on the initial simple index link *)
  let initial_link_simple =
    {host = ""; port = 80; path = "./simple-html/index.html"} in
  let i = crawler num_pages_to_search initial_link_simple in

  let link_html =
    {host = ""; port = 80; path = "./html/index.html"} in
  let j = crawler num_pages_to_search link_html in

  let link_wiki =
    {host = ""; port = 80; path = "./wiki/Teenage_Mutant_Ninja_Turtles"} in
  let k = crawler num_pages_to_search link_wiki in

  (*
    take advantage of partial application & test if
    the word "girls" appears on certain web-pages
   *)
  let test_gabbi = link_present i "girls" in
  let test_the = link_present i "the" in

  let test_leroy = link_present j "Leroy" in
  let test_modules = link_present j "modules" in

  let test_teenage = link_present k "teenage" in
  let test_new = link_present k "new" in

  (* indicate if the tests suceed *)
  if
    test_gabbi "./simple-html/gabbi.html" &&
    not (test_gabbi "./simple-html/index.html") &&
    not (test_gabbi "./simple-html/queue.html") &&
    test_the "./simple-html/sam.html" &&
    test_the "./simple-html/piazza.html" &&
    not (test_the "./simple-html/cs50.html") &&
    test_leroy "./html/index.html" &&
    not (test_leroy "./html/language.html") &&
    test_modules "./html/manual004.html" &&
    not (test_modules "./html/manual003.html") &&
    test_teenage "./wiki/Teenage_Mutant_Ninja_Turtles" &&
    not (test_teenage "./wiki/20th_Century_Fox") &&
    test_new "./wiki/20th_Century_Fox" &&
    test_new "./wiki/Adolescent_Radioactive_Black_Belt_Hamsters" &&
    test_new "./wiki/2012_Kids%27_Choice_Awards"
  then
    Printf.printf "SUCCESS: Crawler tests succeeded!\n"
  else
    Printf.printf "FAILURE: Crawler tests failed.\n"
;;

(* function that times the crawler and prints
   to stdout, uses the cs51 module, returns
   the index  *)
let time_crawler crawler num link =
  (* partially apply crawler to force it to fit
   * in to cs51 timing framework *)
  let partially_applied = crawler num in
  Printf.printf "Crawler timing: ";
  (* this prints out the time results and returns
   * the index *)
  call_reporting_time partially_applied link
;;

(* function that prints out the linkindex for debugging *)
let debug_index (index : LinkIndex.dict) : unit =
  Printf.printf "\nIndex:\n";
  print_string (LinkIndex.string_of_dict index);
  flush_all ()
;;

(* function for debugging pagerank -- karma problem *)
let debug_pageranks (ranks : RankDict.dict) : unit =
    Printf.printf "\nRanks:\n";
    print_string (RankDict.string_of_dict ranks);
    flush_all ()
;;
