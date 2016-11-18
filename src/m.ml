exception UndefSemantics

type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
  | CALLREF of exp * var
  | SET of var * exp
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int 
  | Bool of bool 
  | Closure of var * exp * env 
  | RecClosure of var * var * exp * env
and env = var -> loc
and loc = int
and mem = loc -> value

(*********************************)
(* implementation of environment *)
(*********************************)
(* empty env *)
let empty_env = fun x -> raise (Failure "Environment is empty")
(* extend the environment e with the binding (x,v), where x is a varaible and v is a value *)
let extend_env (x,v) e = fun y -> if x = y then v else (e y)
(* look up the environment e for the variable x *)
let apply_env e x = e x

(*********************************)
(* implementation of memory      *)
(*********************************)
let empty_mem = fun _ -> raise (Failure "Memory is empty")
let extend_mem (l,v) m = fun y -> if l = y then v else (m y)
let apply_mem m l = m l

(* NOTE: you don't need to understand how env and mem work *)

let counter = ref 0

(* calling 'new_location' produces a fresh memory location *)
let new_location () = counter:=!counter+1;!counter

let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Closure (x,e,env) -> "Closure "
  | RecClosure (f,x,e,env) -> "RecClosure "^f

let value2int v =
  match v with
  | Int n -> n
  | _ -> raise UndefSemantics

let value2bool v =
  match v with
  | Bool b -> b
  | _ -> raise UndefSemantics

(* TODO: Implement this function *)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem -> 
  match exp with
  | CONST n -> (Int n, mem)
  | VAR x -> (apply_mem mem (apply_env env x),mem)
  | ADD (arg1, arg2) -> let (v1, m1) = (eval arg1 env mem) in
                                       let (v2, m2) = (eval arg2 env m1) in
                                       (Int (value2int v1 + value2int v2),m2)
  | SUB (arg1, arg2) -> let (v1, m1) = (eval arg1 env mem) in
                                       let (v2, m2) = (eval arg2 env m1) in
                                       (Int (value2int v1 - value2int v2),m2)
  | MUL (arg1, arg2) -> let (v1, m1) = (eval arg1 env mem) in
                                       let (v2, m2) = (eval arg2 env m1) in
                                       (Int (value2int v1 * value2int v2),m2)
  | DIV (arg1, arg2) -> let (v1, m1) = (eval arg1 env mem) in
                                       let (v2, m2) = (eval arg2 env m1) in
                                       (Int (value2int v1 / value2int v2),m2)
  | ISZERO arg -> let (v1, m1) = (eval arg env mem) in
                                 (if (value2int v1) = 0 then (Bool true, m1) else (Bool false, m1))
  | READ -> (Int (read_int()), mem)
  | IF (arg1, arg2, arg3) -> let (v1, m1) = (eval arg1 env mem) in
                                            (if (value2bool v1)
                                            then (eval arg2 env m1)
                                            else (eval arg3 env m1))
  | LET (x, arg1, arg2) -> let (v1, m1) = (eval arg1 env mem) in
                                          let l = new_location() in
                                          let (new_env, new_mem) = ((extend_env (x, l) env), (extend_mem (l, v1) mem)) in
                                          (eval arg2 new_env new_mem)
  | LETREC (f, x, arg1, arg2) -> let l = new_location() in
                                         let (new_env, new_mem) =
                                           ((extend_env (f, l) env), (extend_mem (l, (RecClosure (f, x, arg1, env))) mem)) in
                                         (eval arg2 new_env new_mem)
  | PROC (x, body) -> (Closure (x, body, env), mem)                                        
  | CALL (arg1, arg2) -> let (v1, m1) = (eval arg1 env mem) in
                                        (match v1 with
                                        | Closure (x, body, e) -> (
                                          let (v2, m2) = (eval arg2 env m1) in
                                          let l = new_location() in
                                          let (new_env, new_mem) = ((extend_env (x, l) e), (extend_mem (l, v2) m2)) in
                                          (eval body new_env new_mem)
                                        )
                                        | RecClosure (f, x, body, e) -> (
                                          let (v2, m2) = (eval arg2 env m1) in
                                          let l1 = new_location() in
                                          let l2 = new_location() in
                                          let (new_env, new_mem) = ((extend_env (x, l1) e), (extend_mem (l1, v2) m2)) in
                                          let (new_env, new_mem) =
                                            ((extend_env (f, l2) new_env), (extend_mem (l2, RecClosure (f, x, body, e)) new_mem)) in
                                          (eval body new_env new_mem)
                                        )
                                        | _ -> raise UndefSemantics)
  | CALLREF (arg1, arg2) -> let (v1, m1) = (eval arg1 env mem) in
                                           (match v1 with
                                           | Closure (x, body, e) -> (
                                             let new_env = (extend_env (x, apply_env env arg2) e) in
                                             (eval body new_env m1)
                                           )
                                           | RecClosure (f, x, body, e) -> (
                                             let l = new_location() in
                                             let new_env = (extend_env (x, apply_env env arg2) e) in
                                             let new_env = (extend_env (f, l) new_env) in
                                             let new_mem = (extend_mem (l, RecClosure (f, x, body, e)) m1) in
                                             (eval body new_env new_mem)
                                           )
                                           | _ -> raise UndefSemantics)
  | SET (arg1, arg2) -> let (v1, m1) = (eval arg2 env mem) in
                                       let new_mem = (extend_mem (apply_env env arg1, v1) m1) in
                                       (v1, new_mem)
                                       
  | SEQ (arg1, arg2) -> let (v1, m1) = (eval arg1 env mem) in
                                       (eval arg2 env m1)
  | BEGIN arg -> (eval arg env mem)
  | _ -> raise UndefSemantics

let run : program -> value
=fun pgm -> 
  let (v,m) = eval pgm empty_env empty_mem in
    v
