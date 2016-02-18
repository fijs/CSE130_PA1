exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = 
  match listAssoc (x,evn) with
  | Some value -> value
  | None       -> failwith ("variable not bound: "^x)

let rec eval (evn,e) =
  match e with
  | Const i           -> Int i
  | Var v             -> lookup(v,evn)
  | Bin(e1,binop,e2)  -> (match binop with
                         | Plus  -> (match eval(evn, e1) with
                                    | Int x -> match eval(evn, e2) with
                                               | Int y -> Int (x+y))
                         | Minus -> (match eval(evn, e1) with
                                    | Int x -> match eval(evn, e2) with
                                               | Int y -> Int (x-y))
                         | Mul   -> (match eval(evn, e1) with
                                    | Int x -> match eval(evn, e2) with
                                               | Int y -> Int (x*y))
                         | Div   -> (match eval(evn, e1) with
                                    | Int x -> match eval(evn, e2) with
                                               | Int y -> Int (x/y))
                         | Eq    -> (match eval(evn, e1) with
                                    | Bool x -> (match eval(evn, e2) with
                                                | Bool y -> Bool (x=y)
                                                | _     -> failwith ("You must compare two bools|integers"))
                                    | Int x -> (match eval(evn, e2) with
                                                | Int y -> Bool (x=y)
                                                | _     -> failwith ("You must compare two bools|integers"))
                                    | _     -> failwith ("value not correct type for Eq"))
                         | Ne    -> (match eval(evn, e1) with
                                    | Bool x -> (match eval(evn, e2) with
                                                | Bool y -> Bool (x != y)
                                                | _     -> failwith ("You must compare two bools|integers"))
                                    | Int x -> (match eval(evn, e2) with
                                                | Int y -> Bool (x != y)
                                                | _     -> failwith ("You must compare two bools|integers"))
                                    | _     -> failwith ("value not correct type for Ne"))
                         | Lt   -> (match eval(evn, e1) with
                                    | Int x -> (match eval(evn, e2) with
                                               | Int y -> Bool (x < y)
                                               | _     -> failwith ("values must both be Ints for Lt"))
                                    | _     -> failwith ("values must both be Ints for Lt"))
                         | Le   -> (match eval(evn, e1) with
                                    | Int x -> (match eval(evn, e2) with
                                               | Int y -> Bool (x <= y)
                                               | _     -> failwith ("values must both be Ints for Le"))
                                    | _     -> failwith ("values must both be Ints for Le"))
                         | And    -> (match eval(evn, e1) with
                                     | Bool x -> (match eval(evn, e2) with
                                                 | Bool y -> Bool (x && y)
                                                 | _      -> failwith ("values must both be Bools for And"))
                                     | _      -> failwith ("values must both be Bools for And"))
                         | Or    -> (match eval(evn, e1) with
                                     | Bool x -> (match eval(evn, e2) with
                                                 | Bool y -> Bool (x || y)
                                                 | _      -> failwith ("values must both be Bools for Or"))
                                     | _      -> failwith ("values must both be Bools for Or"))
                         | _      -> failwith ("Invalid operation"))

  | If(e1,e2,e3)      -> (match eval(evn, e1) with
                          | Bool x when x = true  -> eval(evn, e2)
                          | Bool x when x = false -> eval(evn, e3)
                          | _      -> failwith ("value not correct type for If"))

  | Let(x,e1,e2)      -> (let newEvn = (x, eval(evn,e1))::evn in
                            let x = eval(newEvn,e1) in eval(newEvn,e2))

  | Letrec(x,e1,e2)   -> (match e1 with
                         (* Check that the first expression for the let rec statement is a function *)
                         | Fun(fu,e) -> (match eval(evn,e1) with
                                         (* If it is, extract the fields in the closure *)
                                         | Closure(cEvn,fname,fparam,bodyExp) -> 
                                         (* And evaluate by passing into eval a new environment consisting
                                            of evn and a new tuple with the name of the recursive function,
                                            and the Closure value of the function with an updated name.
                                         *)
                                           eval( (x,Closure(cEvn,Some x,fparam,bodyExp))::evn, e2))
                         (* If not a function, behave like let *)
                         | _        -> (let newEvn = (x, eval(evn,e1))::evn in
                                          let x = eval(newEvn,e1) in eval(newEvn,e2))
                         )

                         (* A function should return a closure value *)    
  | Fun (x,e)         -> Closure(evn,None,x,e) 

                          (* Evaluate function to get back a closure value *)
  | App (fn,exp)      -> (match eval(evn, fn) with
                          (* Extract the closure values *)
                          | Closure(cEvn,fname,fparam,bodyExp) -> match fname with
                          (* Add the Closure generated by the recursive function to the environment 
                             used to evaluate the exp in App. Pair the closure with the key generated
                             by the Letrec statement. Add the updated env to the closure environment.
                             Use eval with this updated cEvn to evaluate the body-expression.
                           *)
                          | Some x -> eval( 
                            (x, Closure(cEvn,fname,fparam,bodyExp))::[(fparam,eval(evn,exp))]@cEvn, bodyExp)
                          (* Add the formal parameter and the value of the exp in App to the closure 
                             environment. Then, use the updated closure evn to evaluate the body-expression.
                           *)
                          | None   -> eval( (fparam,eval(evn,exp))::cEvn, bodyExp))

(**********************     Testing Code  ******************************)
