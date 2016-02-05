type token =
  | Num of (int)
  | Id of (string)
  | TRUE
  | FALSE
  | EOF

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
