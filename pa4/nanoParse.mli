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

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
