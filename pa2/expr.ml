(* Fernando I Jaime, A11643783
 * expr.ml
 * cse130 Winter 2016
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)

(* 
   Grammar/datatype to be used when generating/evaluating expressions. 
   Added the Abs type for the Absolute value of (X) function as well 
   as the Sin(X*Y*Z) function.
 *)

type expr = 
    VarX
  | VarY
  | Sine          of expr
  | Cosine        of expr
  | Average       of expr * expr
  | Times         of expr * expr
  | Thresh        of expr * expr * expr * expr
  | Abs           of expr (* Absolute value of (x) *)
  | Sin_XYZ       of expr * expr * expr (* Sin of (X*Y*Z) *)

(*
    Function exprToString : expr -> string
    To enable the printing of expressions to the console.
    Uses pattern matching and string concatenation.
 *)

let rec exprToString e = 
  match e with
  | VarX                            -> "x"
  | VarY                            -> "y"
  | Sine e'                         -> "sin(pi*"^exprToString e'^")"
  | Cosine e'                       -> "cos(pi*"^exprToString e'^")"
  | Average (e1,e2)                 -> "(("^exprToString e1^"+"^exprToString e2^")/2)"
  | Times (e1,e2)                   -> exprToString e1^"*"^exprToString e2
  | Thresh (e1,e2,e3,e4)            -> "("^exprToString e1^"<"^exprToString e2^"?"^exprToString e3^":"^exprToString e4^")"
  | Abs e'                          -> "ABS("^exprToString e'^")"
  | Sin_XYZ (eX,eY,eZ)              -> "sin(pi*"^exprToString eX^"*"^exprToString eY^"*"^exprToString eZ^")"

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                            = VarX
let buildY()                            = VarY
let buildSine(e)                        = Sine(e)
let buildCosine(e)                      = Cosine(e)
let buildAverage(e1,e2)                 = Average(e1,e2)
let buildTimes(e1,e2)                   = Times(e1,e2)
let buildThresh(a,b,a_less,b_less)      = Thresh(a,b,a_less,b_less)
let buildAbs(e)                         = Abs(e)
let buildSin_XYZ(eX,eY,eZ)              = Sin_XYZ(eX,eY,eZ)


let pi = 4.0 *. atan 1.0

(* 
  Function eval : expr * float * float -> float
  Evaluates expressions according to the grammar:
  e ::= x | y | sin (pi*e) | cos (pi*e) | ((e + e)/2) | e * e | (e<e ? e : e) | Abs (e) | sin (pi * e * e * e)
  Recursive function that receives a 3-tuple corresponding to an expression and the x,y coordinates of a point.
  Then uses pattern matching to evaluate the expression according to it's datatype.
 *)

let rec eval (e,x,y) =
  match e with
    | VarX                       -> x
    | VarY                       -> y
    | Sine e'                    -> sin(pi *. (eval (e',x,y)))
    | Cosine e'                  -> cos(pi *. (eval (e',x,y)))
    | Average (e1,e2)            -> ((eval (e1,x,y) +. eval (e2,x,y))/.2.0)
    | Times (e1,e2)              -> eval (e1,x,y) *. eval (e2,x,y)
    | Thresh (e1,e2,e3,e4)       -> if eval (e1,x,y) < eval (e2,x,y) then eval (e3,x,y) else eval (e4,x,y)
    | Abs e'                     -> abs_float (eval (e',x,y))
    | Sin_XYZ (eX,eY,eZ)         -> sin(pi *. ( (eval (eX,x,y)) *. (eval (eY,x,y)) *. (eval (eZ,x,y)) ) )  

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))

(************** Add Testing Code Here ***************)
