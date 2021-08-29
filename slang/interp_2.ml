(**************************************
Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 
(* Interpreter 2. 

A high-level stack-oriented abstract machine with compiler. 
What do I mean by "high-level"? 
---Code is still tree-structured. 
---Complex values are pushed onto value stack.  
---Slang state (heap) used only for references. 
---Code is maintained on a code stack. 
---Program variables contained in code.
*) 


open Ast 

module IntMap = Map.Make(struct type t = int let compare = compare end)

let complain = Errors.complain

type address = int

type var = string

type 'a value =
     | REF of address
     | INT of int
     | BOOL of bool
     | UNIT
     | PAIR of 'a value * 'a value
     | INL of 'a value
     | INR of 'a value
     | CLOSURE of 'a closure
     | REC_CLOSURE of 'a code

and 'a closure = 'a code * 'a env 


and 'a instruction =
  | PUSH of 'a * 'a value
  | LOOKUP of 'a * var
  | UNARY of 'a * Ast.unary_oper
  | OPER of 'a * Ast.oper
  | ASSIGN of 'a
  | SWAP of 'a
  | POP of 'a
  | BIND of 'a * var
  | FST of 'a
  | SND of 'a
  | DEREF of 'a
  | APPLY of 'a
  | MK_PAIR of 'a
  | MK_INL of 'a
  | MK_INR of 'a
  | MK_REF of 'a
  | MK_CLOSURE of 'a * 'a code
  | MK_REC of 'a * var * 'a code
  | TEST of 'a * 'a code * 'a code
  | CASE of 'a * 'a code * 'a code
  | WHILE of 'a * 'a code * 'a code

and 'a code = 'a instruction list

and 'a binding = Ast.var * 'a value

and 'a env = 'a binding list

type 'a env_or_value = EV of 'a env | V of 'a value

type 'a env_value_stack = 'a env_or_value list

(* This is the the slang program state --- that is, values for references *) 
(* It is an array of referenced values together with next unallocated address *)
type 'a state = ('a value IntMap.t) * int

type 'a interp_state = 'a code * 'a env_value_stack * 'a state

(* Printing *) 

let string_of_list sep f l = 
   let rec aux f = function 
     | [] -> ""
     | [t] -> (f t)
     | t :: rest -> (f t) ^  sep  ^ (aux f rest)
   in "[" ^ (aux f l) ^ "]"

let rec string_of_value = function 
     | REF a          -> "REF(" ^ (string_of_int a) ^ ")"
     | BOOL b         -> string_of_bool b
     | INT n          -> string_of_int n 
     | UNIT           -> "UNIT"
     | PAIR(v1, v2)    -> "(" ^ (string_of_value v1) ^ ", " ^ (string_of_value v2) ^ ")"
     | INL v           -> "inl(" ^ (string_of_value v) ^ ")"
     | INR  v          -> "inr(" ^ (string_of_value v) ^ ")"
     | CLOSURE(cl) -> "CLOSURE(" ^ (string_of_closure cl) ^ ")"
     | REC_CLOSURE(c) -> "REC_CLOSURE(" ^ (string_of_code c) ^ ")"

and string_of_closure (c, env) = 
   "(" ^ (string_of_code c) ^ ", " ^ (string_of_env env) ^ ")"

and string_of_env env = string_of_list ",\n " string_of_binding env 

and string_of_binding (x, v) =    "(" ^ x ^ ", " ^ (string_of_value v) ^ ")"

and string_of_instruction = function
 | UNARY(_, op)     -> "UNARY " ^ (string_of_uop op)
 | OPER(_, op)      -> "OPER " ^ (string_of_bop op)
 | MK_PAIR _        -> "MK_PAIR"
 | FST _            -> "FST"
 | SND _            -> "SND"
 | MK_INL _         -> "MK_INL"
 | MK_INR _         -> "MK_INR"
 | MK_REF _         -> "MK_REF"
 | PUSH(_, v)       -> "PUSH " ^ (string_of_value v)
 | LOOKUP(_, x)     -> "LOOKUP " ^ x
 | TEST(_, c1, c2) -> "TEST(" ^ (string_of_code c1) ^ ", " ^ (string_of_code c2) ^ ")"
 | CASE(_, c1, c2) -> "CASE(" ^ (string_of_code c1) ^ ", " ^ (string_of_code c2) ^ ")"
 | WHILE(_, c1, c2) -> "WHILE(" ^ (string_of_code c1) ^ ", " ^ (string_of_code c2) ^ ")"
 | APPLY _       -> "APPLY"
 | BIND(_, x)       -> "BIND " ^ x
 | SWAP _         -> "SWAP"
 | POP _         -> "POP"
 | DEREF _       -> "DEREF"
 | ASSIGN _       -> "ASSIGN"
 | MK_CLOSURE(_, c) -> "MK_CLOSURE(" ^ (string_of_code c) ^ ")"
 | MK_REC(_, f, c) -> "MK_REC(" ^ f ^ ", " ^ (string_of_code c) ^ ")"

and string_of_code c = string_of_list ";\n " string_of_instruction c 

let string_of_env_or_value = function 
  | EV env -> "EV " ^ (string_of_env env)
  | V v -> "V " ^ (string_of_value v)

let string_of_env_value_stack n = string_of_list ";\n " string_of_env_or_value n

let string_of_state (heap, i)  = 
    let rec aux k = 
            if i < k 
	    then "" 
	    else (string_of_int k) ^ " -> " ^ (string_of_value (IntMap.find k heap)) ^ "\n" ^ (aux (k+1))
    in if i = 0
       then ""
       else "\nHeap = \n" ^ (aux 0) 

let string_of_interp_state (c, evs, s) = 
     "\nCode Stack = \n" ^ (string_of_code c) 
     ^ "\nEnv/Value Stack = \n" ^ (string_of_env_value_stack evs) 
     ^ (string_of_state(s)) 

(* The "MACHINE" *) 

(* allocate a new location in the heap
   and give it value v
*) 
let allocate (heap, i) v = 
    if i < Option.heap_max 
    then let heap = IntMap.add i v heap
         in (i, (heap, i+1))
    else complain "runtime error: heap kaput"

let deref (heap, _) a = IntMap.find a heap

let assign (heap, i) a v =
    let heap = IntMap.add a v heap
    in (heap, i) 


(* update : (env * binding) -> env *) 
(* let update(env, (x, v)) = (x, v) :: env *)

let mk_fun(c, env) = CLOSURE(c, env) 
let mk_rec(f, c, env) = CLOSURE(c, (f, REC_CLOSURE(c))::env)

(* 
   in interp_0: 

   interpret(LetRecFun(f, (x, body), e), env) = 
       let rec new_env g = 
           if g = f then FUN (fun v -> interpret(body, update(new_env, (x, v)))) else env g
       in interpret(e, new_env, store) 

      new_env x = env x 
      new_env f = FUN (fun v -> interpret(body, update(new_env, (x, v))))

      lookup (env1 @ [(f, cl1)] @ evn2, f) = 
        CLOSURE (false, (x, body, (f, cl2) :: env2))  
*) 
let lookup_opt (env, x) = 
    let rec aux = function 
      | [] -> None 
      | (y, v) :: rest -> 
          if x = y 
          then Some(match v with 
               | REC_CLOSURE(body) -> mk_rec(x, body, rest)
               | _ -> v)
          else aux rest  
      in aux env 

let rec search (evs, x) = 
  match evs with 
  | [] -> complain (x ^ " is not defined!\n")
  | (V _) :: rest -> search (rest, x) 
  | (EV env) :: rest -> 
    (match lookup_opt(env, x) with 
    | None -> search (rest, x) 
    | Some v -> v 
    ) 

 let rec evs_to_env = function 
  | [] -> []
  | (V _) :: rest -> evs_to_env rest 
  | (EV env) :: rest -> env @ (evs_to_env rest) 
    
    
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

(*
    val step : interp_state -> interp_state 
             = (code * env_value_stack * state) -> (code * env_value_stack * state) 
*) 
let step = function 

(* (code stack,         value/env stack, state) -> (code stack,  value/env stack, state) *)
 | ((PUSH(_, v)) :: ds,                        evs, s) -> (ds, (V v) :: evs, s)
 | ((POP _) :: ds,                        _ :: evs, s) -> (ds, evs, s)
 | ((SWAP _):: ds,                e1 :: e2 :: evs, s) -> (ds, e2 :: e1 :: evs, s)
 | ((BIND(_, x)) :: ds,               (V v) :: evs, s) -> (ds, EV([(x, v)]) :: evs, s)
 | ((LOOKUP(_, x)) :: ds,                      evs, s) -> (ds, V(search(evs, x)) :: evs, s)
 | ((UNARY(_, op)) :: ds,             (V v) :: evs, s) -> (ds, V(do_unary(op, v)) :: evs, s)
 | ((OPER(_, op)) :: ds,   (V v2) :: (V v1) :: evs, s) -> (ds, V(do_oper(op, v1, v2)) :: evs, s)
 | (MK_PAIR(_) :: ds,     (V v2) :: (V v1) :: evs, s) -> (ds, V(PAIR(v1, v2)) :: evs, s)
 | (FST(_) :: ds,           V(PAIR (v, _)) :: evs, s) -> (ds, (V v) :: evs, s)
 | (SND(_) :: ds,           V(PAIR (_, v)) :: evs, s) -> (ds, (V v) :: evs, s)
 | (MK_INL(_) :: ds,                 (V v) :: evs, s) -> (ds, V(INL v) :: evs, s)
 | (MK_INR(_) :: ds,                 (V v) :: evs, s) -> (ds, V(INR v) :: evs, s)
 | (CASE (_, c1,  _) :: ds,       V(INL v) :: evs, s) -> (c1 @ ds, (V v) :: evs, s)
 | (CASE (_, _, c2) :: ds,       V(INR v) :: evs, s) -> (c2 @ ds, (V v) :: evs, s)
 | ((TEST(_, c1, _)) :: ds,   V(BOOL true) :: evs, s) -> (c1 @ ds, evs, s)
 | ((TEST(_, _, c2)) :: ds,  V(BOOL false) :: evs, s) -> (c2 @ ds, evs, s)
 | (ASSIGN(_) :: ds,  (V v) :: (V (REF a)) :: evs, s) -> (ds, V(UNIT) :: evs, assign s a v)
 | (DEREF(_) :: ds,            (V (REF a)) :: evs, s) -> (ds, V(deref s a) :: evs, s)
 | (MK_REF(_) :: ds,                 (V v) :: evs, s) -> let (a, s') = allocate s v in (ds, V(REF a) :: evs, s')
 | ((WHILE(_, _, _)) :: ds,  V(BOOL false) :: evs, s) -> (ds, V(UNIT) :: evs, s)
 | ((WHILE(l, c1, c2)) :: ds, V(BOOL true) :: evs, s) -> (c2 @ [POP l] @ c1 @ [WHILE(l, c1, c2)] @ ds, evs, s)
 | ((MK_CLOSURE(_, c)) :: ds,                  evs, s) -> (ds,  V(mk_fun(c, evs_to_env evs)) :: evs, s)
 | (MK_REC(_, f, c) :: ds,                    evs, s) -> (ds,  V(mk_rec(f, c, evs_to_env evs)) :: evs, s)
 | (APPLY(_) :: ds,  V(CLOSURE (c, env)) :: (V v) :: evs, s)
                                                   -> (c @ ds, (V v) :: (EV env) :: evs, s)
 | state -> complain ("step : bad state = " ^ (string_of_interp_state state) ^ "\n")

let rec driver n state = 
  let _ = if Option.verbose 
          then print_string ("\nState " ^ (string_of_int n) 
                             ^ " : " ^ (string_of_interp_state state) ^ "\n")
          else () 
  in match state with 
     | ([], [V v], s) -> (v, s)  
     | _ -> driver (n + 1) (step state) 


(* A BIND will leave an env on stack. 
   This gets rid of it.  *) 
let leave_scope l = [SWAP l; POP l]

(*
   val compile : expr -> code 
*) 
let rec compile = function
 | Unit l           -> [PUSH(l, UNIT)]
 | Integer(l, n)      -> [PUSH(l, INT n)]
 | Boolean(l, b)      -> [PUSH(l, BOOL b)]
 | Var(l, x)          -> [LOOKUP(l, x)]
 | UnaryOp(l, op, e) -> (compile e) @ [UNARY(l, op)]
 | Op(l, e1, op, e2) -> (compile e1) @ (compile e2) @ [OPER(l, op)]
 | Pair(l, e1, e2)   -> (compile e1) @ (compile e2) @ [MK_PAIR l]
 | Fst(l, e)          -> (compile e) @ [FST l]
 | Snd(l, e)          -> (compile e) @ [SND l]
 | Inl(l, e)          -> (compile e) @ [MK_INL l]
 | Inr(l, e)          -> (compile e) @ [MK_INR l]
 | Case(l, e, (l', x1, e1), (l'', x2, e2)) ->
       (compile e)
       @ [CASE(l, (BIND(l', x1)) :: (compile e1) @ leave_scope l,
               (BIND(l'', x2)) :: (compile e2) @ leave_scope l)]
 | If(l, e1, e2, e3) -> (compile e1) @ [TEST(l, compile e2, compile e3)]
 | Seq(_, [])         -> []
 | Seq(_, [e])        -> compile e
 (* Locations on sequence should highlight entire code blocks? *)
 | Seq(l, (e ::rest)) -> (compile e) @ [POP l] @ (compile (Seq(l, rest)))
 | Ref(l, e)          -> (compile e) @ [MK_REF l]
 | Deref(l, e)        -> (compile e) @ [DEREF l]
 | While(l, e1, e2)  -> let cl = compile e1 in cl @ [WHILE(l, cl, compile e2)]
 | Assign(l, e1, e2) -> (compile e1) @ (compile e2) @ [ASSIGN l]
 | App(l, e1, e2)    -> (compile e2)   (* I chose to evaluate arg first *)
                     @ (compile e1)
                     @ [APPLY l;
                        SWAP l; POP l]  (* get rid of env left on stack *)
 | Lambda(l, x, e)   -> [MK_CLOSURE(l, (BIND(l, x)) :: (compile e) @ leave_scope l)]
 | LetFun(l, f, (l', x, body), e)    ->
       (MK_CLOSURE(l, (BIND(l', x)) :: (compile body) @ leave_scope l)) ::
       (BIND(l, f)) ::
       (compile e) @ leave_scope l
 | LetRecFun(l, f, (l', x, body), e) ->
       (MK_REC(l, f, (BIND(l', x)) :: (compile body) @ leave_scope l)) ::
       (BIND(l, f)) ::
       (compile e) @ leave_scope l


(* The initial Slang state is the Slang state : all locations contain 0 *) 
let initial_state = (IntMap.empty, 0)

let initial_env = [] 

(* interpret : expr -> (value * state) *) 
let interpret e = 
    let c = compile e in 
    let _ = if Option.verbose 
            then print_string("Compile code =\n" ^ (string_of_code c) ^ "\n")
            else () 
    in driver 1 (c, initial_env, initial_state)
