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
%token			LPAREN
%token			RPAREN 
%token 			LBRAC
%token 			RBRAC 
%token 			SEMI 
%token 			COLONCOLON 

%start exp 
%type <Nano.expr> exp

%%

exp: 	LET Id EQ exp IN exp 		{ Let($2,$4,$6) 	}
	|	LET REC Id EQ exp IN exp 	{ Letrec($3,$5,$7)  }
	|   FUN Id ARROW exp 			{ Fun($2,$4) 		}
	|	IF exp THEN exp ELSE exp 	{ If($2,$4,$6) 		}
	|	expOR						{ $1 				}

expOR: 
	| 	expOR OR expAD 		{ Bin($1,Or,$3) }
	| 	expAD 				{ $1 			}

expAD:  expAD AND expCP 	{ Bin($1,And,$3) }
	|	expCP				{ $1 			 }

expCP:  expCP  EQ eCONS		{ Bin($1,Eq,$3) }
	| 	expCP  LT eCONS 	{ Bin($1,Lt,$3) }
	| 	expCP  LE eCONS 	{ Bin($1,Le,$3) }
	| 	expCP  NE eCONS 	{ Bin($1,Ne,$3) }
	|   eCONS				{ $1 			}

eCONS:  expPM COLONCOLON eCONS { Bin($1,Cons,$3) }
	|	LBRAC expPM SEMI eCONS { Bin($2,Cons,$4) }
	|   expPM SEMI eCONS	   { Bin($1,Cons,$3) }
	|	expPM RBRAC 		   { Bin($1,Cons,NilExpr) }	
	|	expPM				   { $1 }

expPM: expPM PLUS  expMD 	{ Bin($1,Plus,$3)  }
	|  expPM MINUS expMD 	{ Bin($1,Minus,$3) }
	|  expMD				{ $1 			   }

expMD: expMD  MUL funAP 	{ Bin($1,Mul,$3) }
	|  expMD  DIV funAP 	{ Bin($1,Div,$3) }
	|  funAP				{ $1 			 }

funAP: funAP atom 			{ App($1,$2) }
	|  atom   				{ $1 		 }

atom:	Num    			 	{ Const($1) }
	|	TRUE   			 	{ True  	}	
	|	FALSE  			 	{ False 	}
	|   Id     				{ Var($1)   }
	| 	LPAREN exp RPAREN 	{ $2 		}
	|	LBRAC RBRAC			{ NilExpr 	}