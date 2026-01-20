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
open Errors

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

type address = int
type var = string

type value =
  | REF of address
  | INT of int
  | BOOL of bool
  | UNIT
  | PAIR of value * value
  | INL of value
  | INR of value
  | CLOSURE of closure
  | REC_CLOSURE of code

and closure = code * env

and instruction =
  | PUSH of value
  | LOOKUP of var
  | UNARY of Ast.unary_oper
  | OPER of Ast.oper
  | ASSIGN
  | SWAP
  | POP
  | BIND of var
  | FST
  | SND
  | DEREF
  | APPLY
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of code
  | MK_REC of var * code
  | TEST of code * code
  | CASE of code * code
  | WHILE of code * code

and code = instruction list
and binding = Ast.var * value
and env = binding list

type env_or_value = EV of env | V of value
type env_value_stack = env_or_value list

(* This is the the slang program state --- that is, values for references *)
(* It is an array of referenced values together with next unallocated address *)
type state = value IntMap.t * int
type interp_state = code * env_value_stack * state

(* Printing *)

let pr = Format.fprintf

let pp_list fmt sep f l =
  let rec aux f fmt = function
    | [] -> ()
    | [ t ] -> f fmt t
    | t :: rest -> pr fmt "%a%(%)%a" f t sep (aux f) rest
  in
  pr fmt "@[[%a]@]" (aux f) l

let rec pp_value fmt = function
  | REF a -> pr fmt "REF(%d)" a
  | BOOL b -> pr fmt "%b" b
  | INT n -> pr fmt "%d" n
  | UNIT -> pr fmt "UNIT"
  | PAIR (v1, v2) -> pr fmt "(%a, %a)" pp_value v1 pp_value v2
  | INL v -> pr fmt "inl(%a)" pp_value v
  | INR v -> pr fmt "inr(%a)" pp_value v
  | CLOSURE cl -> pr fmt "CLOSURE(%a)" pp_closure cl
  | REC_CLOSURE c -> pr fmt "REC_CLOSURE(%a)" pp_code c

and pp_closure fmt (c, env) = pr fmt "(%a, %a)" pp_code c pp_env env
and pp_env fmt env = pp_list fmt ",@\n " pp_binding env
and pp_binding fmt (x, v) = pr fmt "(%s, %a)" x pp_value v

and pp_instruction fmt = function
  | UNARY op -> pr fmt "@[UNARY %s@]" (string_of_uop op)
  | OPER op -> pr fmt "@[OPER %s@]" (string_of_bop op)
  | MK_PAIR -> pr fmt "MK_PAIR"
  | FST -> pr fmt "FST"
  | SND -> pr fmt "SND"
  | MK_INL -> pr fmt "MK_INL"
  | MK_INR -> pr fmt "MK_INR"
  | MK_REF -> pr fmt "MK_REF"
  | PUSH v -> pr fmt "PUSH %a" pp_value v
  | LOOKUP x -> pr fmt "LOOKUP %s" x
  | TEST (c1, c2) -> pr fmt "TEST(@[%a,@ %a)@]" pp_code c1 pp_code c2
  | CASE (c1, c2) -> pr fmt "CASE(@[%a,@ %a)@]" pp_code c1 pp_code c2
  | WHILE (c1, c2) -> pr fmt "WHILE(@[%a,@ %a)@]" pp_code c1 pp_code c2
  | APPLY -> pr fmt "APPLY"
  | BIND x -> pr fmt "BIND %s" x
  | SWAP -> pr fmt "SWAP"
  | POP -> pr fmt "POP"
  | DEREF -> pr fmt "DEREF"
  | ASSIGN -> pr fmt "ASSIGN"
  | MK_CLOSURE c -> pr fmt "MK_CLOSURE(%a)" pp_code c
  | MK_REC (f, c) -> pr fmt "MK_REC(@[%s, %a)@]" f pp_code c

and pp_code fmt c = pp_list fmt ";@\n " pp_instruction c

let pp_env_or_value fmt = function
  | EV env -> pr fmt "EV %a" pp_env env
  | V v -> pr fmt "V %a" pp_value v

let pp_env_value_stack fmt n = pp_list fmt ";@\n " pp_env_or_value n

let pp_state fmt (heap, i) =
  let rec aux fmt k =
    if i > k then
      pr fmt "%d -> %a@\n%a" k pp_value (IntMap.find k heap) aux (k + 1)
  in
  if i <> 0 then
    pr fmt "@\nHeap = @\n%a" aux 0

let pp_interp_state fmt (c, evs, s) =
  pr fmt "@\nCode Stack = @\n%a@\nEnv/Value Stack = @\n%a%a" pp_code c
    pp_env_value_stack evs pp_state s

let string_of_instruction = Format.asprintf "%a" pp_instruction
let string_of_value = Format.asprintf "%a" pp_value
let string_of_env_or_value = Format.asprintf "%a" pp_env_or_value
let string_of_code = Format.asprintf "%a" pp_code

(* The "MACHINE" *)

(* allocate a new location in the heap
   and give it value v
*)
let allocate (heap, i) v =
  if i < Option.heap_max then
    let heap = IntMap.add i v heap in
    (i, (heap, i + 1))
  else
    complain "runtime error: heap kaput"

let deref (heap, _) a = IntMap.find a heap

let assign (heap, i) a v =
  let heap = IntMap.add a v heap in
  (heap, i)

(* update : (env * binding) -> env *)
(* let update(env, (x, v)) = (x, v) :: env *)

let mk_fun (c, env) = CLOSURE (c, env)
let mk_rec (f, c, env) = CLOSURE (c, (f, REC_CLOSURE c) :: env)

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
let rec lookup_opt = function
  | [], _ -> None
  | (y, REC_CLOSURE body) :: rest, x when x = y -> Some (mk_rec (x, body, rest))
  | (y, v) :: _, x when x = y -> Some v
  | (_, _) :: rest, x -> lookup_opt (rest, x)

let rec search (evs, x) =
  match evs with
  | [] -> complainf "%s is not defined!\n" x
  | V _ :: rest -> search (rest, x)
  | EV env :: rest -> (
      match lookup_opt (env, x) with None -> search (rest, x) | Some v -> v)

let rec evs_to_env = function
  | [] -> []
  | V _ :: rest -> evs_to_env rest
  | EV env :: rest -> env @ evs_to_env rest

let readint () =
  let _ = print_string "input> " in
  read_int ()

let do_unary = function
  | NOT, BOOL m -> BOOL (not m)
  | NEG, INT m -> INT (-m)
  | READ, UNIT -> INT (readint ())
  | op, _ -> complainf "malformed unary operator: %s" (string_of_unary_oper op)

let do_oper = function
  | AND, BOOL m, BOOL n -> BOOL (m && n)
  | OR, BOOL m, BOOL n -> BOOL (m || n)
  | EQB, BOOL m, BOOL n -> BOOL (m = n)
  | LT, INT m, INT n -> BOOL (m < n)
  | EQI, INT m, INT n -> BOOL (m = n)
  | ADD, INT m, INT n -> INT (m + n)
  | SUB, INT m, INT n -> INT (m - n)
  | MUL, INT m, INT n -> INT (m * n)
  | DIV, INT m, INT n -> INT (m / n)
  | op, _, _ -> complainf "malformed binary operator: %s" (string_of_oper op)

(*
    val step : interp_state -> interp_state
             = (code * env_value_stack * state) -> (code * env_value_stack * state)
*)
let step = function
  (* (code stack,         value/env stack, state) -> (code stack,  value/env stack, state) *)
  | PUSH v :: ds, evs, s -> (ds, V v :: evs, s)
  | POP :: ds, _ :: evs, s -> (ds, evs, s)
  | SWAP :: ds, e1 :: e2 :: evs, s -> (ds, e2 :: e1 :: evs, s)
  | BIND x :: ds, V v :: evs, s -> (ds, EV [ (x, v) ] :: evs, s)
  | LOOKUP x :: ds, evs, s -> (ds, V (search (evs, x)) :: evs, s)
  | UNARY op :: ds, V v :: evs, s -> (ds, V (do_unary (op, v)) :: evs, s)
  | OPER op :: ds, V v2 :: V v1 :: evs, s ->
      (ds, V (do_oper (op, v1, v2)) :: evs, s)
  | MK_PAIR :: ds, V v2 :: V v1 :: evs, s -> (ds, V (PAIR (v1, v2)) :: evs, s)
  | FST :: ds, V (PAIR (v, _)) :: evs, s -> (ds, V v :: evs, s)
  | SND :: ds, V (PAIR (_, v)) :: evs, s -> (ds, V v :: evs, s)
  | MK_INL :: ds, V v :: evs, s -> (ds, V (INL v) :: evs, s)
  | MK_INR :: ds, V v :: evs, s -> (ds, V (INR v) :: evs, s)
  | CASE (c1, _) :: ds, V (INL v) :: evs, s -> (c1 @ ds, V v :: evs, s)
  | CASE (_, c2) :: ds, V (INR v) :: evs, s -> (c2 @ ds, V v :: evs, s)
  | TEST (c1, _) :: ds, V (BOOL true) :: evs, s -> (c1 @ ds, evs, s)
  | TEST (_, c2) :: ds, V (BOOL false) :: evs, s -> (c2 @ ds, evs, s)
  | ASSIGN :: ds, V v :: V (REF a) :: evs, s -> (ds, V UNIT :: evs, assign s a v)
  | DEREF :: ds, V (REF a) :: evs, s -> (ds, V (deref s a) :: evs, s)
  | MK_REF :: ds, V v :: evs, s ->
      let a, s' = allocate s v in
      (ds, V (REF a) :: evs, s')
  | WHILE (_, _) :: ds, V (BOOL false) :: evs, s -> (ds, V UNIT :: evs, s)
  | WHILE (c1, c2) :: ds, V (BOOL true) :: evs, s ->
      (c2 @ [ POP ] @ c1 @ [ WHILE (c1, c2) ] @ ds, evs, s)
  | MK_CLOSURE c :: ds, evs, s -> (ds, V (mk_fun (c, evs_to_env evs)) :: evs, s)
  | MK_REC (f, c) :: ds, evs, s ->
      (ds, V (mk_rec (f, c, evs_to_env evs)) :: evs, s)
  | APPLY :: ds, V (CLOSURE (c, env)) :: V v :: evs, s ->
      (c @ ds, V v :: EV env :: evs, s)
  | state -> complainf "step : bad state = %a\n" pp_interp_state state

let rec driver n state =
  let () =
    if Option.verbose then
      Format.printf "\nState %d : %a@." n pp_interp_state state
  in
  match state with [], [ V v ], s -> (v, s) | _ -> driver (n + 1) (step state)

(* A BIND will leave an env on stack.
   This gets rid of it.  *)
let leave_scope = [ SWAP; POP ]

(*
   val compile : expr -> code
*)
let rec compile = function
  | Unit -> [ PUSH UNIT ]
  | Integer n -> [ PUSH (INT n) ]
  | Boolean b -> [ PUSH (BOOL b) ]
  | Var x -> [ LOOKUP x ]
  | UnaryOp (op, e) -> compile e @ [ UNARY op ]
  | Op (e1, op, e2) -> compile e1 @ compile e2 @ [ OPER op ]
  | Pair (e1, e2) -> compile e1 @ compile e2 @ [ MK_PAIR ]
  | Fst e -> compile e @ [ FST ]
  | Snd e -> compile e @ [ SND ]
  | Inl e -> compile e @ [ MK_INL ]
  | Inr e -> compile e @ [ MK_INR ]
  | Case (e, (x1, e1), (x2, e2)) ->
      compile e
      @ [
          CASE
            ( (BIND x1 :: compile e1) @ leave_scope,
              (BIND x2 :: compile e2) @ leave_scope );
        ]
  | If (e1, e2, e3) -> compile e1 @ [ TEST (compile e2, compile e3) ]
  | Seq [] -> []
  | Seq [ e ] -> compile e
  (* Locations on sequence should highlight entire code blocks? *)
  | Seq (e :: rest) -> compile e @ [ POP ] @ compile (Seq rest)
  | Ref e -> compile e @ [ MK_REF ]
  | Deref e -> compile e @ [ DEREF ]
  | While (e1, e2) ->
      let cl = compile e1 in
      cl @ [ WHILE (cl, compile e2) ]
  | Assign (e1, e2) -> compile e1 @ compile e2 @ [ ASSIGN ]
  | App (e1, e2) ->
      compile e2 (* I chose to evaluate arg first *)
      @ compile e1 @ [ APPLY; SWAP; POP ]
      (* get rid of env left on stack *)
  | Lambda (x, e) -> [ MK_CLOSURE ((BIND x :: compile e) @ leave_scope) ]
  | LetFun (f, (x, body), e) ->
      MK_CLOSURE ((BIND x :: compile body) @ leave_scope)
      :: BIND f :: compile e
      @ leave_scope
  | LetRecFun (f, (x, body), e) ->
      (MK_REC (f, (BIND x :: compile body) @ leave_scope) :: BIND f :: compile e)
      @ leave_scope

(* The initial Slang state is the Slang state : all locations contain 0 *)
let initial_state = (IntMap.empty, 0)
let initial_env = []

(* interpret : expr -> (value * state) *)
let interpret e =
  let c = compile e in
  let () =
    if Option.verbose then
      Format.printf "Compile code =@\n%a@." pp_code c
  in
  driver 1 (c, initial_env, initial_state)
