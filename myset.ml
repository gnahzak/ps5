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

    struct
    module D = Dict.Make(struct
      type key = C.t
      type value = int
      let compare = C.compare
      let string_of_key = C.string_of_t
      let string_of_value _ = "0"

      let gen_key = C.gen
      let gen_key_random = C.gen_random
      let gen_key_gt = C.gen_gt
      let gen_key_lt = C.gen_lt
      let gen_key_between = C.gen_between
      let gen_value () = 0
      let gen_pair () = (gen_key (), gen_value ())

      end)

    type elt = D.key
    type set = D.dict
    let empty = D.empty

    let is_empty (d : set) = (D.choose d = None)

    let insert (d : set) (k : elt) = D.insert d k 0

    let singleton (k : elt) = D.insert empty k 0

    let fold (f : 'a -> elt -> 'a) (a : 'a) (d : set) =
      let g a k _ = f a k in
      D.fold g a d

    let remove = D.remove

    let member = D.member

    (* add all elements in y to x *)
    let union (x : set) (y : set) =
      let f (x : set) (k : elt) =
        insert x k
      in
      fold f x y

    (* For every element in a, remove it if it is not in b *)
    let intersect (x : set) (y : set) =
      let f (modified : set) (k : elt) =
        if (member y k) then modified
        else (remove modified k)
      in
      fold f x x

    let choose (d : set) =
      match D.choose d with
      | None -> None
      | Some (k, _, newD) -> Some (k, newD)

    let string_of_elt = D.string_of_key
    let string_of_set (s : set) = D.string_of_dict s

    (* Tests for the DictSet functor -- Use the tests from the ListSet
       functor to see how you should write tests. However, you must
       write a lot more comprehensive tests to test ALL your
       functions. *)

    let insert_list (d: set) (lst: elt list) : set =
      List.fold_left (fun r k -> insert r k) d lst

    let rec generate_random_list (size: int) : elt list =
      if size <= 0 then []
      else (C.gen_random ()) :: (generate_random_list (size - 1))

    let rec generate_pos_list (size: int) : elt list =
      if size <= 0 then []
      else (C.gen_gt (C.gen())) :: (generate_pos_list (size - 1))

    let rec generate_neg_list (size: int) : elt list =
      if size <= 0 then []
      else (C.gen_lt (C.gen())) :: (generate_neg_list (size - 1))

    (* Add your test functions to run_tests *)
    let test_isempty () =
      assert(is_empty empty);
      let a = insert empty (C.gen_random()) in
      assert(not (is_empty a));
      ()

    let test_insert () =
      let elts = generate_random_list 50 in
      let s1 = insert_list empty elts in
      List.iter (fun k ->
        assert(member s1 k); assert(D.lookup s1 k = Some 0)) elts;
      ()

    let test_remove () =
      let elts = generate_random_list 50 in
      let s1 = insert_list empty elts in
      let s2 = List.fold_right (fun k r -> remove r k) elts s1 in
      List.iter (fun k -> assert(not (member s2 k))) elts;
      assert (is_empty s2);
      ()

    let test_singleton () =
      let k = C.gen_random() in
      let one = singleton k in
      assert (member one k);
      let zero = remove one k in
      assert (is_empty zero);
      ()

    let test_member () =
      let poslist = generate_pos_list 50 in
      let neglist = generate_neg_list 50 in
      let s = insert_list empty poslist in
      List.iter (fun a -> assert(member s a)) poslist;
      List.iter (fun b -> assert(not (member s b))) neglist;
      ()

    let test_fold () =
      let ftransfer d k = insert d k in
      let elts = generate_random_list 10 in
      let s = insert_list empty elts in
      let s_copy = fold ftransfer empty s in
      List.iter (fun k -> assert( member s_copy k )) elts;

      let fclear d k = remove d k in
      let cleared = fold fclear s s_copy in
      assert (is_empty cleared);
      ()

    let test_union () =
      let el1 = generate_random_list 50 in
      let el2 = generate_random_list 40 in
      let s = union (insert_list empty el1) (insert_list empty el2) in
      let f k = assert (member s k) in
      List.iter f el1;
      List.iter f el2;
      ()

    let test_intersect () =
      let el1 = generate_random_list 50 in
      let el2 = generate_random_list 40 in
      let s1 = insert_list empty el1 in
      let s2 = insert_list empty el2 in
      let u = intersect s1 s2 in
      let f x =
        assert ((member s1 x && member s2 x && member u x) ||
                ((not (member s1 x) || not (member s2 x)) && not (member u x)))
      in
      List.iter f el1;
      List.iter f el2;

      let neglist = generate_neg_list 50 in
      let poslist = generate_pos_list 50 in
      let x = insert_list empty neglist in
      let y = insert_list empty poslist in
      assert (is_empty (intersect x y));
      ()

    let test_choose () =
      let elts = generate_random_list 10 in
      let s = insert_list empty elts in
      match choose s with
      | None -> assert (false);
      | Some (k, d) ->
        assert (member s k && not (member d k));
        match choose d with
        | None -> assert (false);
        | Some (k2, d2) ->
          assert ((member s k2) && (member d k2) && not (member d2 k2));
      ()

    let run_tests () =
      test_isempty();
      test_insert();
      test_singleton();
      test_fold();
      test_remove();
      test_member();
      test_union();
      test_intersect();
      test_choose() ;;

end

(*----------------------------------------------------------------------
  Running the tests.
 *)

(* Create a module for sets of ints using the ListSet functor and test
   it. *)
module IntListSet = ListSet(IntComparable) ;;


(* Create a set of ints using the DictSet functor and test it.

   Uncomment out the lines below when you are ready to test your set
   implementation based on dictionaries. *)

module IntDictSet = DictSet(IntComparable) ;;

let _ = IntDictSet.run_tests();;

(*----------------------------------------------------------------------
  Make -- a functor that creates a set module by calling the ListSet
  or DictSet functors.

  This allows switching between th two implementations for all sets
  just by changing one place in the code.  *)

module Make (C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use the dictionary implementation of sets
     when you are finished. *)
  DictSet (C)
  (* DictSet (C) *)
