(* This file uses some advanced parsing techniques
   to parse juxtaposed applications [e1 e2 e3] the
	 same way as OCaml does. *)

%{
open Ast

(** [make_apply e [e1; e2; ...]] makes the application  
    [e e1 e2 ...]).  Requires: the list argument is non-empty. *)
let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> App (e, e')
	| h :: ((_ :: _) as t) -> make_apply (App (e, h)) t
%}

%token <string> ID
%token FUN ARROW LPAREN RPAREN EOF
%token <int> INT
%token LET
%token BE
%token IN
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token TRUE
%token FALSE
%token OR
%token AND
%token IF
%token THEN
%token ELSE
%token EQUAL
%token GREATER
%token GREATEREQUAL
%token LESS
%token LESSEQUAL

%nonassoc IN
%left OR
%left AND
%left EQUAL
%left GREATER
%left GREATEREQUAL
%left LESS
%left LESSEQUAL
%left PLUS
%left MINUS
%left TIMES
%left DIV

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| e = simpl_expr { e }
	| e = simpl_expr; es = simpl_expr+ { make_apply e es }
	| e1 = expr; PLUS; e2 = expr { Binop (Plus, e1, e2) }
	| e1 = expr; MINUS; e2 = expr { Binop (Minus, e1, e2) }
	| e1 = expr; TIMES; e2 = expr { Binop (Times, e1, e2) }
	| e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) }
	| e1 = expr; OR;  e2 = expr { Binop (Or, e1, e2) }
	| e1 = expr; AND;  e2 = expr { Binop (And, e1, e2) }
	| e1 = expr; EQUAL;  e2 = expr { Binop (Equal, e1, e2) }
	| e1 = expr; GREATER;  e2 = expr { Binop (Greater, e1, e2) }
	| e1 = expr; GREATEREQUAL;  e2 = expr { Binop (GreaterEqual, e1, e2) }
	| e1 = expr; LESS;  e2 = expr { Binop (Less, e1, e2) }
	| e1 = expr; LESSEQUAL;  e2 = expr { Binop (LessEqual, e1, e2) }
	| LET; x = ID; BE; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
	| FUN; x = ID; ARROW; e = expr { Fun (x, e) }
	| IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr { If(c, e1, e2) }
	;

simpl_expr:
    | i = INT { Int i }
    | TRUE { Bool true }
    | FALSE { Bool false }
	| x = ID { Var x }
    | LPAREN; e=expr; RPAREN { e } 
  ;
