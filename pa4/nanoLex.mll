{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

(* comment more *)
rule token = parse
  | "true"		{ TRUE }
  | "false"		{ FALSE }
  | "let"		{ LET }
  | "rec"		{ REC }
  | "="			{ EQ  }
  | "in" 		{ IN  }
  | "fun"		{ FUN }
  | "->"		{ ARROW }
  | "if"		{ IF }
  | "then"		{ THEN }
  | "else"		{ ELSE }
  | "+"			{ PLUS }
  | "-"			{ MINUS }
  | "*"			{ MUL }
  | "/"			{ DIV }
  | "<"			{ LT }
  | "<="		{ LE }
  | "!="		{ NE }
  | "&&"		{ AND }
  |	"||"		{ OR } 
  | ['0'-'9']+ as int1 { Num(int_of_string int1) }
  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str { Id(str) }
  | [' ' '\t' '\n' '\r']    { token lexbuf }
  | eof         { EOF }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
