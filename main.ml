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
  | Bool of bool
  | Int of int
  | Env of env


(** [eval env e] is the [<env, e> ==> v] relation. *)
let rec eval (env : env) (e : expr) : value = match e with
  | Var x -> eval_var env x
  | Bool b -> Bool(b)
  | Int i -> Int(i)
  | Binop (bop, e1, e2) -> begin
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      eval_bop bop v1 v2
    end
  | App (e1, e2) -> eval_app env e1 e2
  | Fun (x, e) -> Closure (x, e, env)
  | Let (x, e) -> let v = eval env e in Env (Env.add x v env)
  | LetIn (x, e1, e2) -> begin
      let v1 = eval env e1 in 
      let new_env = Env.add x v1 env in
      eval new_env e2
    end
  | If (c, e1, e2) -> eval_if env c e1 e2


(** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
and eval_var env x = 
  try Env.find x env with Not_found ->
    failwith (String.concat "Unbound variable: " ["";x])

(** [eval_bop bop e1 e2] is the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop bop v1 v2 = match bop, v1, v2 with
  | Plus, Int a, Int b -> Int (a + b)
  | Minus, Int a, Int b -> Int (a - b)
  | Times, Int a, Int b -> Int (a * b)
  | Div, Int a, Int b -> Int (a / b)
  | Or, Bool a, Bool b -> Bool (a || b)
  | And, Bool a, Bool b -> Bool (a && b)
  | Equal, Int a, Int b -> Bool (a = b)
  | Equal, Bool a, Bool b -> Bool (a = b)
  | Greater, Int a, Int b -> Bool (a > b)
  | GreaterEqual, Int a, Int b -> Bool (a >= b)
  | Less, Int a, Int b -> Bool (a < b)
  | LessEqual, Int a, Int b -> Bool (a <= b)
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

and eval_if env c e1 e2 = 
  match eval env c with
    | Bool true -> eval env e1 
    | Bool false -> eval env e2
    | _ -> failwith "Condition of if must have type bool"


(** [interp s] interprets [s] by parsing
    and evaluating it with the big-step model,
    starting with the empty envir onment. *)
let interp (env: env) (s : string) : value =
  s |> parse |> eval env

let rec string_of_value (v : value) : string =
  match v with
    | Bool b -> string_of_bool b
    | Int i -> string_of_int i
    | Env e -> List.fold_left (fun acc (k, v) -> acc^k^":"^(string_of_value v)^" ") "" (Env.bindings e)
    | Closure (x, e, defenv) -> "fun"

let rec prompt (env: env) = 
  let () = print_string "> " in
  let line = read_line () in begin
      let v = interp env line in
      let () = string_of_value v |> print_endline in
      match v with
      | Closure (_, _, new_env) -> prompt new_env
      | Env new_env -> prompt new_env
      | _ -> prompt env
    end

let _ = prompt Env.empty
