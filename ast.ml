(** The type of binary operators. *)
type bop = 
  | Plus
  | Minus
  | Times
  | Div
  | Or
  | And

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Bool of bool
  | Int of int
  | Binop of bop * expr * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Fun of string * expr
  | If of expr * expr * expr
