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


(* Function sqsum : int list -> int = <fun>
   Gives the sum of the squares of every element in the list.
   Uses List.fold_left to fold function f a x over the list.
   Base case is zero as the sum of no elements is zero.
 *)
let sqsum xs = 
  let f a x = a + (x * x) in
  let base = 0 in
    List.fold_left f base xs

(* Function pipe : ('a -> 'a) list -> 'a -> 'a = <fun>
   The base case is a function which takes an input a and returns a. 
   The function f a x implements a curried function which takes an
   input b and applies function x to the result of applying function
   a to b, recursively.
   Uses List.fold_left to fold function f a x over the list.
 *)
let pipe fs = 
  let f a x = fun b -> x (a b) in
  let base = fun a -> a in
    List.fold_left f base fs

(* Function sepConcat : string -> string list -> string = <fun>
   This function takes a string sep to be used as a separator
   between elements of a list sl. The base cases are:
   [] -> "" and s1 -> s1.
   The recursive case uses List.fold_left to apply the function
   f a x to every element in the list, which concatenates the 
   accumulator input with the separator and the head of the rest
   of the list (x). 
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a^sep^x in
      let base = h in
      let l = t in
        List.fold_left f base l

(* Function stringOfList : ('a -> string) -> 'a list -> string = <fun>
   This function is implemented through calls to sepConcat using the 
   usual list separator "; ", and passing a call to (List.map f l) as
   the list argument to sepConcat. We concatenate the results of those
   two function calls to starting and ending brackets so we get back 
   the list in string form. 
 *)
let stringOfList f l = "["^(sepConcat "; " (List.map f l))^"]"
(* why does this not work and the above does? recursion! sepConcat ";" (List.map f l) *)


(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)


(* Function clone : 'a -> int -> 'a list = <fun>
   This function takes in an object x and clones it n times into a list
   by using a recursive helper function. The base case is for n to be 
   <= 0, in which case [] is returned. The recursive case involves using
   helper to recursively cons x to an accumulator list l n-1 times.
 *)
let rec clone x n =
  let rec helper x n l =
    if n = 0 then x::l
    else helper x (n-1) (x::l)
  in
  if n <= 0 then []
  else helper x (n-1) []  

(* Function padZero : int list -> int list -> int list * int list = <fun>
   Takes two lists and if they are not of equal length, pads the shorter
   list with zeros at the front using clone with arguments 0 and diff. 
   The result of the call to clone is appended to the passed in list using
   List.append, while diff is simply the length difference between the two lists.
   After padding, the function returns both lists as a tuple.
 *)
let rec padZero l1 l2 = 
  let diff = List.length l1 - List.length l2 in
  if diff < 0 then (List.append ( clone 0 (-1*diff) ) l1, l2)
  else if diff > 0 then (l1, List.append ( clone 0 (diff) ) l2)
  else (l1, l2) 

(* Function removeZero : int list -> int list = <fun>
   The function recursively breaks down the list l
   by pattern matching and using the h::t operation.
   As long as the head of the list continues to be zero,
   the function will recurse. Once h != 0, the tail of 
   the list is returned. If the entire list is zeroes, 
   the empty list is returned.
 *)
let rec removeZero l = 
  match l with
  | [] -> []
  | h::t -> ( if h = 0 then removeZero t else l )

(* Function bigAdd : int list -> int list -> int list = <fun>
   The function uses a helper function "add" that takes in 
   two int lists as a tuple. Add uses List.fold_left
   to apply the function f a x to every element. The function
   f breaks down every new tuple in the list x into x1 and x2
   and sums them into the variable r. The input a is split 
   into carry and sum, and we match sum  with either the 
   empty list for the base case or a list for the 
   recursive case. 
 *)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x =
      let (x1,x2) = x in
        let r = x1 + x2 in
      match a with
      | (carry, sum) -> match sum with
        (* Store both carry and remainder in the list *)
        | []   -> ( r/10 , (r/10)::[r mod 10] )
        (* H is the carry from the previous operation, use it to 
          computer the new sum and carry, then discard *)
        | h::t -> ( (r+carry)/10 , ((r+h)/10)::([(r+h) mod 10]@t)  ) 
    in
    let base = (0,[]) in
    let args = List.rev (List.combine l1 l2) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* Function mulByDigit : int -> int list -> int list = <fun>
   The function uses a helper function to recursively call 
   bidAdd on the original list i number of times. 
 *)
let rec mulByDigit i l = 
  let rec helper i l og_l = 
    if i > 1 then helper (i-1) (bigAdd l og_l) og_l
      else l
  in
  if i <= 0 then []
    else helper i l l

(* Function bigMul : int list -> int list -> int list = <fun>
 *)
let bigMul l1 l2 = 
  let f a x = 

  in
  let base = (0,[]) in
  let args = l2 in
  let (_, res) = List.fold_left f base args in
    res
