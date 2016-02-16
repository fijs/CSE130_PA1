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
  | LPAREN
  | RPAREN
  | LBRAC
  | RBRAC
  | SEMI
  | COLONCOLON

open Parsing;;
let _ = parse_error;;
# 2 "nanoParse.mly"
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
# 39 "nanoParse.ml"
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
  279 (* LPAREN *);
  280 (* RPAREN *);
  281 (* LBRAC *);
  282 (* RBRAC *);
  283 (* SEMI *);
  284 (* COLONCOLON *);
    0|]

let yytransl_block = [|
  257 (* Num *);
  258 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\003\000\
\003\000\004\000\004\000\004\000\004\000\004\000\005\000\005\000\
\005\000\005\000\005\000\006\000\006\000\006\000\007\000\007\000\
\007\000\008\000\008\000\009\000\009\000\009\000\009\000\009\000\
\009\000\000\000"

let yylen = "\002\000\
\006\000\007\000\004\000\006\000\001\000\003\000\001\000\003\000\
\001\000\003\000\003\000\003\000\003\000\001\000\003\000\004\000\
\003\000\002\000\001\000\003\000\003\000\001\000\003\000\003\000\
\001\000\002\000\001\000\001\000\001\000\001\000\001\000\003\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\028\000\031\000\029\000\030\000\000\000\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\014\000\
\000\000\000\000\000\000\027\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\000\000\000\000\
\000\000\026\000\000\000\000\000\000\000\000\000\032\000\000\000\
\000\000\000\000\010\000\011\000\012\000\013\000\000\000\000\000\
\017\000\015\000\000\000\000\000\000\000\000\000\003\000\000\000\
\016\000\000\000\000\000\000\000\001\000\000\000\004\000\002\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000"

let yysindex = "\018\000\
\007\255\000\000\000\000\000\000\000\000\000\000\011\255\029\255\
\007\255\007\255\003\255\000\000\022\255\016\255\015\255\000\000\
\070\255\004\255\045\255\000\000\047\255\054\255\048\255\050\255\
\033\255\034\255\000\000\000\255\049\255\049\255\049\255\049\255\
\049\255\049\255\045\255\045\255\000\000\049\255\049\255\045\255\
\045\255\000\000\007\255\057\255\007\255\007\255\000\000\049\255\
\016\255\015\255\000\000\000\000\000\000\000\000\004\255\004\255\
\000\000\000\000\045\255\045\255\058\255\007\255\000\000\052\255\
\000\000\007\255\061\255\007\255\000\000\007\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\135\000\169\000\148\000\000\000\
\133\000\067\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\172\000\154\000\000\000\000\000\000\000\000\000\089\000\111\000\
\000\000\000\000\023\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\249\255\000\000\042\000\043\000\044\000\068\000\245\255\002\000\
\237\255"

let yytablesize = 452
let yytable = "\042\000\
\025\000\024\000\025\000\003\000\004\000\005\000\006\000\003\000\
\004\000\005\000\006\000\007\000\021\000\035\000\036\000\008\000\
\022\000\009\000\001\000\040\000\041\000\031\000\023\000\055\000\
\056\000\010\000\048\000\026\000\027\000\010\000\023\000\011\000\
\032\000\033\000\034\000\061\000\030\000\063\000\064\000\042\000\
\042\000\059\000\060\000\029\000\024\000\003\000\004\000\005\000\
\006\000\003\000\004\000\005\000\006\000\043\000\067\000\044\000\
\047\000\045\000\069\000\027\000\071\000\046\000\072\000\062\000\
\068\000\066\000\022\000\010\000\070\000\026\000\049\000\010\000\
\050\000\011\000\051\000\052\000\053\000\054\000\028\000\000\000\
\000\000\057\000\058\000\035\000\036\000\000\000\000\000\000\000\
\020\000\000\000\000\000\065\000\000\000\000\000\000\000\037\000\
\038\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\008\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\
\025\000\000\000\000\000\000\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\025\000\000\000\
\025\000\000\000\025\000\025\000\025\000\023\000\023\000\000\000\
\000\000\000\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\000\000\023\000\000\000\
\023\000\023\000\023\000\024\000\024\000\000\000\000\000\000\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\024\000\024\000\000\000\024\000\000\000\024\000\024\000\
\024\000\022\000\022\000\000\000\000\000\000\000\022\000\022\000\
\022\000\022\000\000\000\000\000\022\000\022\000\022\000\022\000\
\022\000\000\000\022\000\000\000\022\000\022\000\022\000\020\000\
\020\000\000\000\000\000\000\000\020\000\020\000\020\000\020\000\
\000\000\000\000\020\000\020\000\020\000\020\000\020\000\000\000\
\020\000\000\000\020\000\020\000\020\000\021\000\021\000\000\000\
\000\000\000\000\021\000\021\000\021\000\021\000\000\000\000\000\
\021\000\021\000\021\000\021\000\021\000\000\000\021\000\000\000\
\021\000\021\000\021\000\019\000\019\000\000\000\005\000\000\000\
\019\000\019\000\005\000\005\000\000\000\000\000\019\000\019\000\
\019\000\019\000\019\000\009\000\019\000\000\000\005\000\009\000\
\009\000\008\000\000\000\000\000\000\000\008\000\008\000\000\000\
\009\000\009\000\000\000\009\000\000\000\000\000\008\000\008\000\
\007\000\008\000\000\000\006\000\007\000\007\000\000\000\006\000\
\006\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\007\000\006\000\000\000\006\000"

let yycheck = "\019\000\
\000\000\009\000\010\000\001\001\002\001\003\001\004\001\001\001\
\002\001\003\001\004\001\005\001\002\001\014\001\015\001\009\001\
\006\001\011\001\001\000\016\001\017\001\007\001\000\000\035\000\
\036\000\023\001\027\001\025\001\026\001\023\001\002\001\025\001\
\018\001\019\001\020\001\043\000\021\001\045\000\046\000\059\000\
\060\000\040\000\041\000\022\001\000\000\001\001\002\001\003\001\
\004\001\001\001\002\001\003\001\004\001\007\001\062\000\002\001\
\024\001\010\001\066\000\026\001\068\000\012\001\070\000\007\001\
\013\001\008\001\000\000\023\001\008\001\025\001\029\000\023\001\
\030\000\025\001\031\000\032\000\033\000\034\000\011\000\255\255\
\255\255\038\000\039\000\014\001\015\001\255\255\255\255\255\255\
\000\000\255\255\255\255\048\000\255\255\255\255\255\255\026\001\
\027\001\028\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\255\255\026\001\027\001\028\001\007\001\008\001\255\255\
\255\255\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\255\255\
\026\001\027\001\028\001\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\255\255\026\001\027\001\
\028\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\255\255\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\255\255\026\001\027\001\028\001\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\255\255\026\001\027\001\028\001\007\001\008\001\255\255\
\255\255\255\255\012\001\013\001\014\001\015\001\255\255\255\255\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\255\255\
\026\001\027\001\028\001\007\001\008\001\255\255\008\001\255\255\
\012\001\013\001\012\001\013\001\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\008\001\024\001\255\255\024\001\012\001\
\013\001\008\001\255\255\255\255\255\255\012\001\013\001\255\255\
\021\001\022\001\255\255\024\001\255\255\255\255\021\001\022\001\
\008\001\024\001\255\255\008\001\012\001\013\001\255\255\012\001\
\013\001\255\255\255\255\255\255\255\255\255\255\022\001\255\255\
\024\001\022\001\255\255\024\001"

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
  LPAREN\000\
  RPAREN\000\
  LBRAC\000\
  RBRAC\000\
  SEMI\000\
  COLONCOLON\000\
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
# 41 "nanoParse.mly"
                             ( Let(_2,_4,_6) 	)
# 293 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 42 "nanoParse.mly"
                             ( Letrec(_3,_5,_7)  )
# 302 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 43 "nanoParse.mly"
                         ( Fun(_2,_4) 		)
# 310 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Nano.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 44 "nanoParse.mly"
                             ( If(_2,_4,_6) 		)
# 319 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expOR) in
    Obj.repr(
# 45 "nanoParse.mly"
              ( _1 				)
# 326 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expOR) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expAD) in
    Obj.repr(
# 48 "nanoParse.mly"
                     ( Bin(_1,Or,_3) )
# 334 "nanoParse.ml"
               : 'expOR))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expAD) in
    Obj.repr(
# 49 "nanoParse.mly"
              ( _1 			)
# 341 "nanoParse.ml"
               : 'expOR))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expAD) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expCP) in
    Obj.repr(
# 51 "nanoParse.mly"
                         ( Bin(_1,And,_3) )
# 349 "nanoParse.ml"
               : 'expAD))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expCP) in
    Obj.repr(
# 52 "nanoParse.mly"
            ( _1 			 )
# 356 "nanoParse.ml"
               : 'expAD))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expCP) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eCONS) in
    Obj.repr(
# 54 "nanoParse.mly"
                         ( Bin(_1,Eq,_3) )
# 364 "nanoParse.ml"
               : 'expCP))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expCP) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eCONS) in
    Obj.repr(
# 55 "nanoParse.mly"
                     ( Bin(_1,Lt,_3) )
# 372 "nanoParse.ml"
               : 'expCP))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expCP) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eCONS) in
    Obj.repr(
# 56 "nanoParse.mly"
                     ( Bin(_1,Le,_3) )
# 380 "nanoParse.ml"
               : 'expCP))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expCP) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eCONS) in
    Obj.repr(
# 57 "nanoParse.mly"
                     ( Bin(_1,Ne,_3) )
# 388 "nanoParse.ml"
               : 'expCP))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eCONS) in
    Obj.repr(
# 58 "nanoParse.mly"
              ( _1 			)
# 395 "nanoParse.ml"
               : 'expCP))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expPM) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eCONS) in
    Obj.repr(
# 60 "nanoParse.mly"
                               ( Bin(_1,Cons,_3) )
# 403 "nanoParse.ml"
               : 'eCONS))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expPM) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'eCONS) in
    Obj.repr(
# 61 "nanoParse.mly"
                          ( Bin(_2,Cons,_4) )
# 411 "nanoParse.ml"
               : 'eCONS))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expPM) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eCONS) in
    Obj.repr(
# 62 "nanoParse.mly"
                         ( Bin(_1,Cons,_3) )
# 419 "nanoParse.ml"
               : 'eCONS))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expPM) in
    Obj.repr(
# 63 "nanoParse.mly"
                    ( Bin(_1,Cons,NilExpr) )
# 426 "nanoParse.ml"
               : 'eCONS))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expPM) in
    Obj.repr(
# 64 "nanoParse.mly"
               ( _1 )
# 433 "nanoParse.ml"
               : 'eCONS))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expPM) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expMD) in
    Obj.repr(
# 66 "nanoParse.mly"
                          ( Bin(_1,Plus,_3)  )
# 441 "nanoParse.ml"
               : 'expPM))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expPM) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expMD) in
    Obj.repr(
# 67 "nanoParse.mly"
                       ( Bin(_1,Minus,_3) )
# 449 "nanoParse.ml"
               : 'expPM))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expMD) in
    Obj.repr(
# 68 "nanoParse.mly"
             ( _1 			   )
# 456 "nanoParse.ml"
               : 'expPM))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expMD) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'funAP) in
    Obj.repr(
# 70 "nanoParse.mly"
                         ( Bin(_1,Mul,_3) )
# 464 "nanoParse.ml"
               : 'expMD))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expMD) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'funAP) in
    Obj.repr(
# 71 "nanoParse.mly"
                      ( Bin(_1,Div,_3) )
# 472 "nanoParse.ml"
               : 'expMD))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'funAP) in
    Obj.repr(
# 72 "nanoParse.mly"
             ( _1 			 )
# 479 "nanoParse.ml"
               : 'expMD))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'funAP) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 74 "nanoParse.mly"
                     ( App(_1,_2) )
# 487 "nanoParse.ml"
               : 'funAP))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 75 "nanoParse.mly"
               ( _1 		 )
# 494 "nanoParse.ml"
               : 'funAP))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 77 "nanoParse.mly"
                  ( Const(_1) )
# 501 "nanoParse.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "nanoParse.mly"
               ( True  	)
# 507 "nanoParse.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "nanoParse.mly"
               ( False 	)
# 513 "nanoParse.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "nanoParse.mly"
                ( Var(_1)   )
# 520 "nanoParse.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Nano.expr) in
    Obj.repr(
# 81 "nanoParse.mly"
                       ( _2 		)
# 527 "nanoParse.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "nanoParse.mly"
                 ( NilExpr 	)
# 533 "nanoParse.ml"
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
