(**************************************
Compiler Construction 2015
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 

(*  Interpreter 0 for Slang.2 

    This is a "definitional" interpreter for  for Slang.2 (the defined language) 
    using high-level constructs of Ocaml (the defining language). 
    For examples, Slang.2 functions are represented as Ocaml functions 
    of type 

           value -> value
           
    Slang conditionals are translated to Ocaml conditionals, etc. 
    The most interesting (and tricky) case is the "let rec" construct of 
    Slang --- this is translated using the "lec rec" construct of Ocaml.
    Not with the defined function itself, but with the definition of 
    a recursive environment! (Because when a recursive function 
    calls itself, it must find its own definition in the environment...) 

    Note that some of the functions can fail.  However, 
    if the input expressin has passed static analysis, then such "run time" 
    errors should never happen! (Can you prove that?) 
*) 
open Ast 

let complain = Errors.complain

type address = int 

type store = address -> value 

and value = 
     | REF of address 
     | INT of int 
     | BOOL of bool 
     | UNIT
     | PAIR of value * value 
     | INL of value 
     | INR of value 
     | FUN of ((value * store) -> (value * store)) 

type env = var -> value 

(* auxiliary functions *) 

let rec string_of_value = function 
     | REF a -> "address(" ^ (string_of_int a) ^ ")"
     | BOOL b -> string_of_bool b
     | INT n -> string_of_int n 
     | UNIT -> "()"
     | PAIR(v1, v2) -> "(" ^ (string_of_value v1) ^ ", " ^ (string_of_value v2) ^ ")"
     | INL v -> "inl(" ^ (string_of_value v) ^ ")"
     | INR  v -> "inr(" ^ (string_of_value v) ^ ")"
     | FUN _ -> "FUNCTION( ... )" 
    
(* update : (env * binding) -> env 
   update : (store * (address * value)) -> store
*) 
let update(env, (x, v)) = fun y -> if x = y then v else env y

let readint () = let _ = print_string "input> " in read_int() 

let do_unary = function 
  | (NOT,  BOOL m) -> BOOL (not m)
  | (NEG,  INT m)  -> INT (-m)
  | (READ, UNIT)   -> INT (readint())
  | (op, _) -> complain ("malformed unary operator: " ^ (string_of_unary_oper op))

let do_oper = function 
  | (AND,  BOOL m,  BOOL n) -> BOOL (m && n)
  | (OR,   BOOL m,  BOOL n) -> BOOL (m || n)
  | (EQB,  BOOL m,  BOOL n) -> BOOL (m = n)
  | (LT,   INT m,   INT n)  -> BOOL (m < n)
  | (EQI,  INT m,   INT n)  -> BOOL (m = n)
  | (ADD,  INT m,   INT n)  -> INT (m + n)
  | (SUB,  INT m,   INT n)  -> INT (m - n)
  | (MUL,  INT m,   INT n)  -> INT (m * n)
  | (DIV,  INT m,   INT n)  -> INT (m / n)
  | (op, _, _)  -> complain ("malformed binary operator: " ^ (string_of_oper op))

let do_deref = function 
  | (REF a, store) -> (store a, store) 
  | (_, _)  -> complain "deref expecting address"

let next_address = ref 0 

let new_address () = let a = !next_address in (next_address := a + 1; a) 

let do_ref = function 
  | (v, store) -> let a = new_address () in (REF a, update(store, (a, v)))

let do_assign a = function 
  | (v, store) -> (UNIT, update(store, (a, v)))

(*
    interpret : (expr * env * store) -> (value * store) 
              : (expr * (var -> value) * address -> value) -> value
*)
let rec interpret (e, env, store) =
    match e with
    | Unit _             -> (UNIT, store)
    | Var(_, x)            -> (env x, store)
    | Integer(_, n)        -> (INT n, store)
    | Boolean( _, b)        -> (BOOL b, store)

    | Seq(_, [])           -> (UNIT, store) (* should not be seen ... *)
    | Seq(_, [e])          -> interpret (e, env, store)
    | Seq(l, (e :: rest))  -> let (_,  store1) = interpret(e, env, store)
                          in interpret(Seq(l, rest), env, store1)
    | While(l, e1, e2)   -> let (v, store') = interpret(e1, env, store) in
                          (match v with
                          | BOOL true -> interpret(Seq(l, [e2; e]), env, store')
                          | BOOL false -> (UNIT, store')
                          | _ -> complain "runtime error.  Expecting a boolean!"
                          )
    | Ref(_, e)            -> do_ref(interpret(e, env, store))
    | Deref(_, e)          -> do_deref(interpret(e, env, store))
    | Assign(_, e1, e2)   -> (match interpret(e1, env, store) with
                           | (REF a, store') -> do_assign a (interpret(e2, env, store'))
                           | _ -> complain "runtime error : expecting an address on left side of assignment"
                           )
    | UnaryOp(_, op, e)   -> let (v, store') = interpret(e, env, store) in (do_unary(op, v), store')
    | Op(_, e1, op, e2)   -> let (v1, store1) = interpret(e1, env, store) in
                          let (v2, store2) = interpret(e2, env, store1) in (do_oper(op, v1, v2), store2)
    | If(_, e1, e2, e3)   -> let (v, store') = interpret(e1, env, store) in
                          (match v with
                          | BOOL true -> interpret(e2, env, store')
                          | BOOL false -> interpret(e3, env, store')
                          | _ -> complain "runtime error.  Expecting a boolean!"
                          )
    | Pair(_, e1, e2)     -> let (v1, store1) = interpret(e1, env, store) in
                          let (v2, store2) = interpret(e2, env, store1) in (PAIR(v1, v2), store2)
    | Fst(_, e)            -> (match interpret(e, env, store) with
                           | (PAIR (v1, _), store') -> (v1, store')
                           | (_, _) -> complain "runtime error.  Expecting a pair!"
                          )
    | Snd(_, e)            -> (match interpret(e, env, store) with
                           | (PAIR (_, v2), store') -> (v2, store')
                           | (_, _) -> complain "runtime error.  Expecting a pair!"
                          )
    | Inl(_, e)            -> let (v, store') = interpret(e, env, store) in (INL v, store')
    | Inr(_, e)            -> let (v, store') = interpret(e, env, store) in (INR v, store')
    | Case(_, e, (_, x1, e1), (_, x2, e2)) ->
      let (v, store') = interpret(e, env, store) in
       (match v with
       | INL v' -> interpret(e1, update(env, (x1, v')), store')
       | INR v' -> interpret(e2, update(env, (x2, v')), store')
       | _ -> complain "runtime error.  Expecting inl or inr!"
       )
    | Lambda(_, x, e)     -> (FUN (fun (v, s) -> interpret(e, update(env, (x, v)), s)), store)
    | App(_, e1, e2)      -> let (v2, store1) = interpret(e2, env, store) in
                          let (v1, store2) =  interpret(e1, env, store1) in
                           (match v1 with
                           | FUN f -> f (v2, store2)
                           | _ -> complain "runtime error.  Expecting a function!"
                          )
    | LetFun(_, f, (_, x, body), e) ->
       let new_env = update(env, (f, FUN (fun (v, s) -> interpret(body, update(env, (x, v)), s))))
       in interpret(e, new_env, store)
    | LetRecFun(_, f, (_, x, body), e) ->
       let rec new_env g = (* a recursive environment! *)
           if g = f then FUN (fun (v, s) -> interpret(body, update(new_env, (x, v)), s)) else env g
       in interpret(e, new_env, store) 

(* env_empty : env *) 
let empty_env = fun x -> complain (x ^ " is not defined!\n")

(* store_empty : env *) 
let empty_store = fun x -> complain ((string_of_int x) ^ " is not allocated!\n")

(* interpret_top_level : expr -> value *) 
let interpret_top_level e = let (v, _) = interpret(e, empty_env, empty_store) in v 
 