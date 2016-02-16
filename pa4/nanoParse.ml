type token =
  | Num of (int)
  | Id of (string)
  | TRUE
  | FALSE
  | EOF
  | LET
  | REC
  | EQ
  | IN
  | FUN
  | ARROW
  | IF
  | THEN
  | ELSE
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LT
  | LE
  | NE
  | AND
  | OR

open Parsing;;
let _ = parse_error;;
# 2 "nanoParse.mly"
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
# 33 "nanoParse.ml"
let yytransl_const = [|
  259 (* TRUE *);
  260 (* FALSE *);
    0 (* EOF *);
  261 (* LET *);
  262 (* REC *);
  263 (* EQ *);
  264 (* IN *);
  265 (* FUN *);
  266 (* ARROW *);
  267 (* IF *);
  268 (* THEN *);
  269 (* ELSE *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* MUL *);
  273 (* DIV *);
  274 (* LT *);
  275 (* LE *);
  276 (* NE *);
  277 (* AND *);
  278 (* OR *);
    0|]

let yytransl_block = [|
  257 (* Num *);
  258 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\006\000\007\000\004\000\006\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\017\000\020\000\018\000\019\000\000\000\000\000\
\000\000\021\000\000\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\000\000\000\000\000\000\000\000\001\000\
\000\000\004\000\002\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000"

let yysindex = "\255\255\
\003\255\000\000\000\000\000\000\000\000\000\000\016\255\000\255\
\003\255\000\000\043\255\000\000\006\255\008\255\005\255\009\255\
\027\255\027\255\027\255\027\255\027\255\027\255\027\255\027\255\
\027\255\027\255\003\255\013\255\003\255\003\255\043\255\043\255\
\043\255\043\255\043\255\043\255\043\255\043\255\043\255\043\255\
\015\255\003\255\000\000\011\255\003\255\018\255\003\255\000\000\
\003\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\009\000\
\011\000\017\000\019\000\025\000\027\000\033\000\035\000\041\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\007\000\049\000\000\000"

let yytablesize = 310
let yytable = "\001\000\
\005\000\015\000\015\000\003\000\004\000\005\000\006\000\007\000\
\006\000\028\000\007\000\008\000\027\000\009\000\029\000\016\000\
\008\000\013\000\009\000\042\000\030\000\014\000\045\000\047\000\
\010\000\049\000\011\000\003\000\004\000\005\000\006\000\000\000\
\012\000\041\000\013\000\043\000\044\000\000\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\046\000\017\000\000\000\048\000\000\000\050\000\000\000\051\000\
\018\000\019\000\020\000\021\000\022\000\023\000\024\000\025\000\
\026\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\015\000\000\000\005\000\005\000\015\000\015\000\
\006\000\000\000\007\000\000\000\006\000\006\000\007\000\007\000\
\008\000\000\000\009\000\000\000\008\000\008\000\009\000\009\000\
\010\000\000\000\011\000\000\000\010\000\010\000\011\000\011\000\
\012\000\000\000\013\000\000\000\012\000\012\000\013\000\013\000\
\014\000\000\000\000\000\000\000\014\000\014\000"

let yycheck = "\001\000\
\000\000\002\001\000\000\001\001\002\001\003\001\004\001\005\001\
\000\000\002\001\000\000\009\001\007\001\011\001\010\001\009\000\
\000\000\002\001\000\000\007\001\012\001\006\001\008\001\013\001\
\000\000\008\001\000\000\001\001\002\001\003\001\004\001\255\255\
\000\000\027\000\000\000\029\000\030\000\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\042\000\007\001\255\255\045\000\255\255\047\000\255\255\049\000\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\017\000\018\000\019\000\020\000\021\000\022\000\023\000\
\024\000\025\000\026\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\008\001\255\255\008\001\255\255\012\001\013\001\012\001\013\001\
\008\001\255\255\008\001\255\255\012\001\013\001\012\001\013\001\
\008\001\255\255\008\001\255\255\012\001\013\001\012\001\013\001\
\008\001\255\255\008\001\255\255\012\001\013\001\012\001\013\001\
\008\001\255\255\008\001\255\255\012\001\013\001\012\001\013\001\
\008\001\255\255\255\255\255\255\012\001\013\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  EOF\000\
  LET\000\
  REC\000\
  EQ\000\
  IN\000\
  FUN\000\
  ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  LT\000\
  LE\000\
  NE\000\
  AND\000\
  OR\000\
  "

let yynames_block = "\
  Num\000\
  Id\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 35 "nanoParse.mly"
                             ( Let(_2,_4,_6) 	)
# 227 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 36 "nanoParse.mly"
                             ( Letrec(_3,_5,_7)  )
# 236 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 37 "nanoParse.mly"
                         ( Fun(_2,_4) 		)
# 244 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Nano.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 38 "nanoParse.mly"
                             ( If(_2,_4,_6) 		)
# 253 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 39 "nanoParse.mly"
               ( _1 )
# 260 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 41 "nanoParse.mly"
                             ( Bin(_1,Plus,_3) )
# 268 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 42 "nanoParse.mly"
                         ( Bin(_1,Minus,_3) )
# 276 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 43 "nanoParse.mly"
                        ( Bin(_1,Mul,_3) )
# 284 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 44 "nanoParse.mly"
                        ( Bin(_1,Div,_3) )
# 292 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 45 "nanoParse.mly"
                       ( Bin(_1,Lt,_3) )
# 300 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 46 "nanoParse.mly"
                       ( Bin(_1,Le,_3) )
# 308 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 47 "nanoParse.mly"
                       ( Bin(_1,Ne,_3) )
# 316 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 48 "nanoParse.mly"
                        ( Bin(_1,And,_3) )
# 324 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 49 "nanoParse.mly"
                       ( Bin(_1,Or,_3) )
# 332 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExp) in
    Obj.repr(
# 50 "nanoParse.mly"
                      ( Bin(_1,Eq,_3) )
# 340 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 51 "nanoParse.mly"
                ( _1 )
# 347 "nanoParse.ml"
               : 'binExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 53 "nanoParse.mly"
             ( Const(_1) )
# 354 "nanoParse.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "nanoParse.mly"
          ( True  )
# 360 "nanoParse.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "nanoParse.mly"
          ( False )
# 366 "nanoParse.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "nanoParse.mly"
            ( Var(_1)   )
# 373 "nanoParse.ml"
               : 'atom))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Nano.expr)
