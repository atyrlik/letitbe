(** The type of binary operators. *)
type bop = 
  | Plus
  | Minus
  | Times
  | Div
  | Or
  | And
  | Equal
  | Greater
  | GreaterEqual
  | Less
  | LessEqual

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Bool of bool
  | Int of int
  | Binop of bop * expr * expr
  | App of expr * expr
  | LetIn of string * expr * expr
  | LetRecIn of string * string * expr * expr
  | LetRec of string * string * expr
  | Let of string * expr
  | Fun of string * expr
  | RecFun of string * string * expr
  | If of expr * expr * expr
