%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int>  	Num
%token <string> Id
%token  		TRUE FALSE
%token 			EOF
%token			LET 
%token			REC
%token			EQ
%token 		 	IN  
%token		 	FUN 
%token		 	ARROW 
%token		 	IF 
%token		 	THEN 
%token		 	ELSE 
%token			PLUS
%token			MINUS
%token			MUL 
%token			DIV 
%token			LT 
%token			LE 
%token			NE 
%token			AND 
%token			OR 

%start exp 
%type <Nano.expr> exp

%%

exp: 	LET Id EQ exp IN exp 		{ Let($2,$4,$6) 	}
	|	LET REC Id EQ exp IN exp 	{ Letrec($3,$5,$7)  }
	|   FUN Id ARROW exp 			{ Fun($2,$4) 		}
	|	IF exp THEN exp ELSE exp 	{ If($2,$4,$6) 		}
	|	binExp						{ $1 }

binExp:  binExp PLUS binExp 	{ Bin($1,Plus,$3) }
	| 	binExp MINUS binExp 	{ Bin($1,Minus,$3) }
	| 	binExp MUL binExp 		{ Bin($1,Mul,$3) }
	| 	binExp DIV binExp 		{ Bin($1,Div,$3) }
	| 	binExp LT binExp 		{ Bin($1,Lt,$3) }
	| 	binExp LE binExp 		{ Bin($1,Le,$3) }
	| 	binExp NE binExp 		{ Bin($1,Ne,$3) }
	| 	binExp AND binExp 		{ Bin($1,And,$3) }
	| 	binExp OR binExp 		{ Bin($1,Or,$3) }
	| 	binExp EQ	binExp		{ Bin($1,Eq,$3) }
	| 	atom   					{ $1 }

atom:	Num    { Const($1) }
	|	TRUE   { True  }	
	|	FALSE  { False }
	|   Id     { Var($1)   }