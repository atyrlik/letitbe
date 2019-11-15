open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [Env] is module to help with environments, which 
    are maps that have strings as keys. *)
module Env = Map.Make(String)

(** [env] is the type of an environment, which maps
    a string to a value. *)
type env = value Env.t

(** [value] is the type of a lambda calculus value.
    In the environment model, that is a closure. *)
and value = 
  | Closure of string * expr * env
  | Int of int


(** [eval env e] is the [<env, e> ==> v] relation. *)
let rec eval (env : env) (e : expr) : value = match e with
  | Var x -> eval_var env x
  | Int i -> Int(i)
  | Binop (bop, e1, e2) -> begin
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      eval_bop bop v1 v2
    end
  | App (e1, e2) -> eval_app env e1 e2
  | Fun (x, e) -> Closure (x, e, env)
  | Let (x, e1, e2) -> begin
      let v1 = eval env e1 in 
      let new_env = Env.add x v1 env in
      eval new_env e2
    end

(** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
and eval_var env x = 
  try Env.find x env with Not_found -> failwith "Unbound variable"

(** [eval_bop bop e1 e2] is the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop bop v1 v2 = match bop, v1, v2 with
  | Add, Int a, Int b -> Int (a + b)
  | _ -> failwith "Operator and operand type mismatch"

(** [eval_app env e1 e2] is the [v] such that [<env, e1 e2> ==> v]. *)
and eval_app env e1 e2 = 
  match eval env e1 with
  | Closure (x, e, defenv) -> begin
      let v2 = eval env e2 in
      let base_env_for_body = defenv in
      let env_for_body = Env.add x v2 base_env_for_body in
      eval env_for_body e
    end
  | _ -> failwith "Only function can be applied to values"

(** [interp s] interprets [s] by parsing
    and evaluating it with the big-step model,
    starting with the empty envir onment. *)
let interp (s : string) : value =
  s |> parse |> eval Env.empty
