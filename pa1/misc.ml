(* CSE 130: Programming Assignment 1
 * Fernando I Jaime, A11643783	
 * misc.ml
 *)


(* sumList : int list -> int 
   A function to sum the integers in a list.
*) 
let rec sumList l : int = 
	match l with
	| [] -> 0
	| x::l -> x + sumList(l);;


(*	digitsNext : int -> int list -> int list
	Helper function to do the heavy lifting for digitsOfInt.
*)
let rec digitsNext n accList : int list = 
	if n <= 9 then n::accList
	else digitsNext (n/10) ( (n mod 10)::accList );;


(*	digitsOfInt : int -> int list 
	Gets the digits of an integer by calling digitsNext,
	and returns the digits as an int list.   	
 *)
let rec digitsOfInt n : int list = digitsNext n [];;


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n);;


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(*	additivePersistence : int -> int 	
	Inductive case: If n is greater than 9, add 1 and call the
	recursive function by passing the sum of the digits of n,
	until n <= than 9.
	Base case: If n is <= 9, AP is zero, and no recursive
	call is needed.
*)
let rec additivePersistence n = 
	match n > 9 with
	| true -> 1 + additivePersistence (sumList (digits n))
	| false -> 0;;


(*	digitalRoot : int -> int 	
	Base case: If n is <= 9, return n.
	Inductive case: If n is greater than 9, call the
	recursive function by passing the sum of the digits of n,
	until n <= than 9.
*)
let rec digitalRoot n = 
	match n <= 9 with
	| true -> n
	| false -> digitalRoot (sumList (digits n));;


(*	appendList : a' list * a' list -> a' list 	
	Base case: l1 is empty, return l2.
	Inductive case: Deconstruct l1 using pattern match,
	taking the head of the list and using cons with a 
	recursive call to the function, passing the tail of
	l1 and l2 in its entirety.
*)
let rec appendList l1 l2 = match l1 with
	| [] -> l2
  	(* head::tail = l1*)
	| (head::tail) -> head::(appendList tail l2);;


(*	listReverse : a' list -> a' list 	
	Base case: l is empty, return an empty list.
	Inductive case: Use pattern match to split l,
	then pass the head and the tail to the recursive 
	call to appendList.
*)
let rec listReverse l =
	match l with
	| [] -> []
	| x::l -> appendList (listReverse l) [x];; 


(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0


(*	palindrome : string -> bool 	
	This function takes a string W and converts it
	into two char lists by calling the explode function.
	l1 conserves the order of the chars in W, and l2 
	reverses it. Then we compares each character in each 
	list and return a boolean.
*)
let palindrome w = 
	let l1 = explode w in
		let l2 = listReverse (explode w) in
			match l1 = l2 with
			| true -> true
			| false -> false;;

(************** Add Testing Code Here ***************)







