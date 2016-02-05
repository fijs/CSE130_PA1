{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
  | [' ' '\t' '\n' '\r']    { token lexbuf }
  | eof         { EOF }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
  | ['0'-'9']+ as int1 { Num(int_of_string int1)}
  | "true"		{ TRUE }
  | "false"		{ FALSE }
  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str { Id(str) }
  

