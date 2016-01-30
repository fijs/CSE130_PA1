(* CSE 130: Programming Assignment 3 WI16
 * Fernando I Jaime
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

let sqsum xs = 
  let f a x = a + (x * x) in
  let base = 0 in
    List.fold_left f base xs

let pipe fs = 
  let f a x = fun b -> x (a b) in
  let base = fun a -> a in
    List.fold_left f base fs

(* val pipe : (('a -> 'a) -> 'a) list -> 'a -> 'a = <fun> *)

let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a^sep^x in
      let base = h in
      let l = t in
        List.fold_left f base l

let stringOfList f l = "["^(sepConcat "; " (List.map f l))^"]"
(* why does this not work and the above does? recursion! sepConcat ";" (List.map f l) *)

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

let rec clone x n = failwith "to be implemented" 

let rec padZero l1 l2 = failwith "to be implemented"

let rec removeZero l = failwith "to be implemented"

let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = failwith "to be implemented" in
    let base = failwith "to be implemented" in
    let args = failwith "to be implemented" in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

let rec mulByDigit i l = failwith "to be implemented"

let bigMul l1 l2 = 
  let f a x = failwith "to be implemented" in
  let base = failwith "to be implemented" in
  let args = failwith "to be implemented" in
  let (_, res) = List.fold_left f base args in
    res
