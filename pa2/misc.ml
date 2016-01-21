(* CSE 130: Programming Assignment 2
   Fernando I Jaime, A11643783
 * misc.ml
 *)

(* UBER-HELPER FUNCTION
let rec fold f elements_so_far elements_remaining k = 
  match elements_remaining with
  | [] -> elements_so_far
  | head::tail -> fold f (f elements_so_far head k) tail k;;
*)

(* HELPER FOR ASSOC 
let assocHelper d k (r1,r2) = 
  if r1 = k then r2
  else d;;
*)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

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

(* Removes duplicates from a list using tail recursion and List.mem *)
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


(* Applies the function f to parameter b, obtaining a pair (b',c').
   Continues to apply f to subsequent b' values until c' is no longer true.
*)
let rec wwhile (f,b) = 
  let (b',c') = f b in 
  if c' = true then wwhile (f,b')
  else b';;


(* fill in the code wherever it says : failwith "to be written" 
  wwhile ((failwith "to be written"),b)
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
