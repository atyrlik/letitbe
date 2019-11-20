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
  | RecClosure of string * string * expr * env
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
  | RecFun (f, x, e) -> RecClosure (f, x, e, env)
  | Let (x, e) -> let v = eval env e in Env (Env.add x v env)
  | LetRec (f, x, e1, e2) -> begin
      let v1 = eval env (RecFun (f, x, e1)) in
      let new_env = Env.add f v1 env in
      eval new_env e2
    end
  | LetIn (x, e1, e2) -> begin
      let v1 = eval env e1 in 
      let new_env = Env.add x v1 env in
      eval new_env e2
    end
  | If (c, e1, e2) -> eval_if env c e1 e2


(** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
and eval_var env x = 
  try Env.find x env with Not_found ->
    failwith ("Unbound variable: "^x)

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
  | RecClosure (f, x, e, defenv) as f_rec -> begin
      let sub_e = subst (subst e e2 x) (RecFun (f, x, e)) f in
      eval env sub_e
    end
  | _ -> failwith "Only function can be applied to values"

and eval_if env c e1 e2 = 
  match eval env c with
    | Bool true -> eval env e1 
    | Bool false -> eval env e2
    | _ -> failwith "Condition of if must have type bool"

(** [subst e v x] is [e] with [v] substituted for [x], that is, [e{v/x}]. *)
and subst e v x = match e with
  | Var y -> if x = y then v else e
  | Bool _ -> e
  | Int _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | App (e1, e2) -> App (subst e1 v x, subst e2 v x)
  | Fun (y, e1) -> Fun (y, subst e1 v x)
  | RecFun (f, y, e1) -> RecFun (f, y, subst e1 v x)
  | Let (y, e1) -> Let (y, subst e1 v x)
  | LetIn (y, e1, e2) ->
    let e1' = subst e1 v x in
    if x = y
    then LetIn (y, e1', e2)
    else LetIn (y, e1', subst e2 v x)
  | LetRec (f, y, e1, e2) ->
    let e1' = subst e1 v x in
    if x = y
    then LetRec (f, y, e1', e2)
    else LetRec (f, y, e1', subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)
  | _ -> e


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
    | Closure (_, _, _) -> "fun"
    | RecClosure (_, _, _, _) -> "rec fun"

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
