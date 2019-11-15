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

%nonassoc IN
%left PLUS

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| e = simpl_expr { e }
	| e = simpl_expr; es = simpl_expr+ { make_apply e es }
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| LET; x = ID; BE; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
	| FUN; x = ID; ARROW; e = expr { Fun (x, e) }
	;

simpl_expr:
    | i = INT { Int i }
	| x = ID { Var x }
    | LPAREN; e=expr; RPAREN { e } 
  ;
