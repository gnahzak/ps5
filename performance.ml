
module AT = Askshiebs_tests ;;
module CR = Crawl ;;
module CS = Crawler_services ;;
module HS = Http_services ;;

open Webtypes ;;
open Pagerank ;;
open Query ;;

(* must pass in number of pages to search, initial link *)
let crawlDir = AT.time_crawler CR.crawler ;;

(* modified version of gen_q_HTML to output time of one query *)
let query_timer (query_string : string) (index: LinkIndex.dict)
               (ranks : RankDict.dict) (root_dir: string) : float =
  let start = Unix.gettimeofday () in
  let query = Q.parse_query query_string in
  let links = Q.eval_query index query in
  let _num = LinkSet.fold (fun l _x -> 1 + l) 0 links in
  let sorted_links = HS.sort_by_rank links ranks in
  let _response_body = HS.html_of_urllist sorted_links ranks root_dir in
  let finish = Unix.gettimeofday () in
  finish -. start ;;

(* for each link, time how long each query takes on average *)
(* i.e. for every key in the dict, see how long it takes to query it *)

let timeq (index : LinkIndex.dict)
          (ranks : RankDict.dict)
          (root_dir : string) : float =
  let f (sum : float) (k : string) (_v : LinkSet.set) : float =
    sum +. (query_timer ("?q=" ^ k) index ranks root_dir)
  in
  LinkIndex.fold f 0.0 index ;;

let test () =
  (* call crawlDir for all three possible directories, and print time *)

  let link_simple =
    {host = ""; port = 80; path = "./simple-html/index.html"} in
  let link_html =
    {host = ""; port = 80; path = "./html/index.html"} in
  let link_wiki =
    {host = ""; port = 80; path = "./wiki/Teenage_Mutant_Ninja_Turtles"} in

  Printf.printf "Crawling tests: \n" ;

  Printf.printf "Simple link: \n" ;
  let simple = crawlDir 8 link_simple in
  let ranks_simple = CS.compute_pagerank simple in

  Printf.printf "HTML link: \n" ;
  let html = crawlDir 20 link_html in
  let ranks_html = CS.compute_pagerank html in

  Printf.printf "Wiki link: \n" ;
  let wiki = crawlDir 224 link_wiki in
  let ranks_wiki = CS.compute_pagerank wiki in

  (* determine amount of time it takes, for each LinkIndex, to
  query each word once *)
  Printf.printf "Query tests: \n" ;

  Printf.printf "Simple html: %f\n" (timeq simple ranks_simple "./simple-html");
  Printf.printf "Html: %f\n" (timeq html ranks_html "./html");
  Printf.printf "Wiki: %f\n" (timeq wiki ranks_wiki "./wiki");

  () ;;

test () ;;

