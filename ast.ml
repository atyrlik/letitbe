(** The type of binary operators. *)
type bop = 
  | Add

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Int of int
  | Binop of bop * expr * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Fun of string * expr