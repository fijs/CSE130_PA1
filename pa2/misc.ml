(* CSE 130: Programming Assignment 2 Winter 2016
 * Fernando I Jaime, A11643783
 * misc.ml
 *)


(* 
    Function assoc : int * string * (string * int) list -> int
    The function receives a 3-tuple in the form of a default int
    value, a string k, and a list of (string,int) tuples. 
    The function will recursively traverse the list with the aid
    of a helper function, comparing the string values of the 
    tuples in the string to the string k. If a match is found
    the int value of that tuple is returned, otherwise, the default
    int value d is returned.
 *)

let rec assoc (d,k,l) = 
  let rec helper k default_value elements_remaining = 
    match elements_remaining with
    (* Will return value d if no k_i matches k *)
    | [] -> default_value
    | head::tail -> 
    ( 
      let (k_i,v_i) = head in
      (* Value found and we can exit recursion *)
      if k_i = k then v_i
      (* Otherwise continue *)
      else helper k default_value tail
    )
  in 
  helper k d l;; 

(*
    Function removeDuplicates : int list -> int list 
    Removes duplicates from a list using tail recursion and List.mem 

    Note: Simply use a helper function to achieve tail-recursion.
    Then use List.mem to see if the current element of the remaining
    elements already exists in our "seen" list. If it exists, do not
    modify the seen list. If it doesn't exist, add it to the head of 
    the seen list. Because we add to the head (or append the list to
    our current element), the initial recursive call happens within 
    a call to List.rev, so that we get the new list in the order of
    the original list.

 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
      | [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
  List.rev (helper ([],l));;


(* 
   Function wwhile : (int -> int * bool) * int -> int
   This recursive function takes a tuple consisting of a function (f) that takes an 
   int (b) and returns an int (b'), until a condition c is no longer true. 
*)
let rec wwhile (f,b) = 
  (* Save the result of the call f (b) in each recursive call as a tuple with the
     modified value of b (b') and the new value of c (c'). *)
  let (b',c') = f b in 
  if c' = true then wwhile (f,b')
  else b';;


(* 
    Function fixpoint : (int -> int) * int -> int
    This function takes in a tuple consisting of a function (f) that returns an int,
    and an int value (b). It recursively applies f to b using a helper function, 
    until f (b) = b.
*)
let fixpoint (f,b) =  
  let rec helper f b =
    if f b = b then b
    else helper f (f b)
  in
  helper f b;;


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
