(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

An interface and simple implementation of a set abstract datatype.
 *)

open Order ;;

(* Interface for set modules *)

module type SET =
  sig
    (* type of elements in the set *)
    type elt

    (* abstract type for the set *)
    type set

    val empty : set

    val is_empty : set -> bool

    val insert : set -> elt -> set

    val singleton : elt -> set

    val union : set -> set -> set

    val intersect : set -> set -> set

    (* remove an element from the set -- if the element isn't present,
      returns set unchanged *)
    val remove : set -> elt -> set

    (* returns true iff the element is in the set *)
    val member : set -> elt -> bool

    (* chooses some member from the set, removes it and returns that
       element plus the new set.  If the set is empty, returns
       None. *)
    val choose : set -> (elt * set) option

    (* fold a function across the elements of the set in some
       unspecified order, using the calling convention of fold_left,
       that is, if the set s contains s1, ..., sn, then
          fold f u s
       returns
          (f ... (f (f u s1) s2) ... sn)
     *)
    val fold : ('a -> elt -> 'a) -> 'a -> set -> 'a

    (* functions to convert values of these types to a string
       representation; useful for debugging. *)
    val string_of_set : set -> string
    val string_of_elt : elt -> string

    (* runs the tests. See TESTING EXPLANATION *)
    val run_tests : unit -> unit
  end

(* COMPARABLE signature -- A module that provides for elements that
   can be compared as to ordering and converted to a string
   representation. Includes functinos for generating values for
   testing purposes.
 *)

module type COMPARABLE =
  sig
    type t
    val compare : t -> t -> ordering
    val string_of_t : t -> string

    (* The functions below are used for testing. See TESTING EXPLANATION *)

    (* Generate a value of type t. The same t is always returned *)
    val gen : unit -> t

    (* Generate a random value of type t. *)
    val gen_random : unit -> t

    (* Generate a t greater than the argument. *)
    val gen_gt : t -> t

    (* Generate a t less than the argument. *)
    val gen_lt : t -> t

    (* Generate a t between the two arguments. Return None if no such
       t exists. *)
    val gen_between : t -> t -> t option
  end

(* An example implementation of the COMPARABLE signature. Use this
   struct for testing. *)

module IntComparable : COMPARABLE =
  struct
    type t = int
    let compare x y =
      if x < y then Less
      else if x > y then Greater
      else Equal
    let string_of_t = string_of_int
    let gen () = 0
    let gen_random =
      let _ = Random.self_init () in
      (fun () -> Random.int 10000)
    let gen_gt x = x + 1
    let gen_lt x = x - 1
    let gen_between x y =
      let (lower, higher) = (min x y, max x y) in
      if higher - lower < 2 then None else Some (higher - 1)
  end

(*----------------------------------------------------------------------
  Implementation 1: List-based implementation of sets, represented as
  sorted lists with no duplicates.
 *)

module ListSet (C: COMPARABLE) : (SET with type elt = C.t) =
  struct
    type elt = C.t
    type set = elt list

    (* INVARIANT: sorted, no duplicates *)
    let empty = []

    let is_empty xs =
      match xs with
      | [] -> true
      | _ -> false

    let singleton x = [x]

    let rec insert xs x =
      match xs with
      | [] -> [x]
      | y :: ys ->
          match C.compare x y with
          | Greater -> y :: (insert ys x)
          | Equal -> xs
          | Less -> x :: xs

    let union xs ys = List.fold_left insert xs ys

    let rec remove xs y =
      match xs with
      | [] -> []
      | x :: xs1 -> 
          match C.compare y x with
          | Equal -> xs1
          | Less -> xs
          | Greater -> x :: (remove xs1 y)

    let rec intersect xs ys =
      match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh :: xt, yh :: yt -> 
          match C.compare xh yh with
          | Equal -> xh :: (intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt

    let rec member xs x =
      match xs with
      | [] -> false
      | y :: ys ->
          match C.compare x y with
          | Equal -> true
          | Greater -> member ys x
          | Less -> false

    let choose xs =
      match xs with
      | [] -> None
      | x :: rest -> Some (x, rest)

    let fold = List.fold_left

    let string_of_elt = C.string_of_t

    let string_of_set (s: set) : string =
      let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
      "set([" ^ (List.fold_left f "" s) ^ "])"


    (* Tests for the ListSet functor -- These are just examples of
    tests, your tests should be a lot more thorough than these. *)

    (* adds a list of (key,value) pairs in left-to-right order *)
    let insert_list (d: set) (lst: elt list) : set =
      List.fold_left (fun r k -> insert r k) d lst

    let rec generate_random_list (size: int) : elt list =
      if size <= 0 then []
      else (C.gen_random ()) :: (generate_random_list (size - 1))

    let test_insert () =
      let elts = generate_random_list 100 in
      let s1 = insert_list empty elts in
      List.iter (fun k -> assert(member s1 k)) elts;
      ()

    let test_remove () =
      let elts = generate_random_list 100 in
      let s1 = insert_list empty elts in
      let s2 = List.fold_right (fun k r -> remove r k) elts s1 in
      List.iter (fun k -> assert(not (member s2 k))) elts;
      ()

    let test_union () =
      ()

    let test_intersect () =
      ()

    let test_member () =
      ()

    let test_choose () =
      ()

    let test_fold () =
      ()

    let test_is_empty () =
      ()

    let test_singleton () =
      ()

    let run_tests () =
      test_insert () ;
      test_remove () ;
      test_union () ;
      test_intersect () ;
      test_member () ;
      test_choose () ;
      test_fold () ;
      test_is_empty () ;
      test_singleton () ;
      ()

  end

(*----------------------------------------------------------------------
  Implementation 2: Sets as dictionaries
 *)
(*
  TODO: Use the skeleton code for the DictSet module below and
  complete the implementation, making sure that it conforms to the
  appropriate signature.

  Add appropriate tests for the functor and make sure that your
  implementation passes the tests. Once you have the DictSet functor
  working, you can use it instead of the ListSet implementation by
  updating the definition of the Make functor below.
*)
module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
    ListSet(C)
(*
    struct
    module D = Dict.Make(struct
        (* fill this in! *)
      end)

    type elt = D.key
    type set = D.dict
    let empty = ???

    (* implement the rest of the functions in the signature! *)

    let string_of_elt = D.string_of_key
    let string_of_set s = D.string_of_dict s

    (* Tests for the DictSet functor -- Use the tests from the ListSet
       functor to see how you should write tests. However, you must
       write a lot more comprehensive tests to test ALL your
       functions. *)

    (* Add your test functions to run_tests *)
    let run_tests () =
      ()
end
*)

(*----------------------------------------------------------------------
  Running the tests.
 *)

(* Create a module for sets of ints using the ListSet functor and test
   it. *)
module IntListSet = ListSet(IntComparable) ;;


(* Create a set of ints using the DictSet functor and test it.

   Uncomment out the lines below when you are ready to test your set
   implementation based on dictionaries. *)

(*
module IntDictSet = DictSet(IntComparable) ;;

let _ = IntDictSet.run_tests();;
 *)

(*----------------------------------------------------------------------
  Make -- a functor that creates a set module by calling the ListSet
  or DictSet functors.

  This allows switching between th two implementations for all sets
  just by changing one place in the code.  *)

module Make (C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use the dictionary implementation of sets
     when you are finished. *)
  ListSet (C)
  (* DictSet (C) *)
