(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

The crawler, which builds a dictionary from words to sets of
links.
 *)

(* Rename modules for convenience *)
module WT = Webtypes ;;
module CS = Crawler_services ;;

(* Only look at pagerank if you plan on implementing it! *)
module PR = Pagerank ;;

(*----------------------------------------------------------------------
  Section 1: CRAWLER
 *)

(* TODO: Replace the implementation of the crawl function (currently
   just a stub returning the empty dictionary) with a proper index of
   crawled pages. Build an index as follows:

   Remove a link from the frontier (the set of links that have yet to
   be visited), visit this link, add its outgoing links to the
   frontier, and update the index so that all words on this page are
   mapped to linksets containing this url.

   Keep crawling until we've reached the maximum number of links (n) or
   the frontier is empty.
 *)

let rec crawl (n : int)
          (frontier : WT.LinkSet.set)
          (visited : WT.LinkSet.set)
          (d : WT.LinkIndex.dict)
        : WT.LinkIndex.dict =

  (* do all of the following if there are pages and links left*)
  if (n > 0) then
    (* remove a link from the frontier; if it is empty, return d *)
    match (WT.LinkSet.choose frontier) with
    | None -> d
    | Some (mem, newFrontier) ->
      (* add the link to the visited set*)
      let newVisited = WT.LinkSet.insert visited mem in
      match Crawler_services.get_page mem with
      (* if the link is broken, ignore/remove it and keep crawling *)
      | None -> crawl n newFrontier visited d
      | Some webpage ->
        (* update index so all words are properly matched *)
        let f (d : WT.LinkIndex.dict) (w : string) =
          let newSet =
            match WT.LinkIndex.lookup d w with
            | None -> WT.LinkSet.insert WT.LinkSet.empty webpage.url
            | Some linkset -> WT.LinkSet.insert linkset webpage.url
          in
          WT.LinkIndex.insert d w newSet
        in
        let newD = List.fold_left f d webpage.words in
        (* add all linked webpages to frontier, if they aren't visited *)
        let addLinks (fron : WT.LinkSet.set) (n : WT.link) =
          if (WT.LinkSet.member visited n) then fron
          else (WT.LinkSet.insert fron n)
        in
        let newFrontier = WT.LinkSet.fold addLinks newFrontier webpage.links in
        crawl (n - 1) newFrontier newVisited newD
  else d ;;

let crawler (num_pages_to_search : int) (initial_link : WT.link) =
  crawl num_pages_to_search
    (WT.LinkSet.singleton initial_link)
    WT.LinkSet.empty
    WT.LinkIndex.empty ;;
