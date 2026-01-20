(**************************************
Compiler Construction 2016
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

(** Interpreter 2.

    A high-level stack-oriented abstract machine with compiler. What do I mean
    by "high-level"?

    --- Code is still tree-structured.

    --- Complex values are pushed onto value stack.

    --- Slang state (heap) used only for references.

    --- Code is maintained on a code stack.

    --- Program variables contained in code. *)

type value = f Value.t
and f = Closure of closure | Rec_closure of Ast.var * closure
and closure = code * env

and instruction =
  | PUSH of value
  | LOOKUP of Ast.var
  | UNARY of Ast.Unary_op.t
  | OPER of Ast.Binary_op.t
  | ASSIGN
  | SWAP
  | POP
  | BIND of Ast.var
  | FST
  | SND
  | DEREF
  | APPLY
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of code
  | MK_REC of Ast.var * code
  | TEST of code * code
  | CASE of code * code
  | WHILE of code * code

and code = instruction list
and binding = Ast.var * value
and env = binding list [@@deriving show { with_path = false }]

type env_or_value = EV of env | V of value
[@@deriving show { with_path = false }]

type stack = env_or_value list [@@deriving show { with_path = false }]

type state = { code : code; stack : stack; heap : value Heap.t }
(** This is the the slang program state --- that is, values for references. It
    is an array of referenced values together with next unallocated address *)

(* Printing *)

let pp_state ppf { code; stack; heap } =
  Format.fprintf ppf
    "@[<v>Code Stack:@;<1 2>%a@;Env/Value Stack:@;<1 2>%a@;Heap:@;<1 2>%a@;@]"
    pp_code code pp_stack stack (Heap.pp pp_value) heap

(* The "MACHINE" *)

let rec search x : stack -> value = function
  | [] -> Errors.complainf "%s is not defined!" x
  | V _ :: rest -> search x rest
  | EV env :: rest -> (
      match List.assoc_opt x env with None -> search x rest | Some v -> v)

let rec evs_to_env : stack -> env = function
  | [] -> []
  | V _ :: rest -> evs_to_env rest
  | EV env :: rest -> env @ evs_to_env rest

(** val step : (code * env_value_stack * state) -> (code * env_value_stack *
    state) *)
let step ({ code; stack; heap } as state) =
  let advance code stack = { state with code; stack } in
  match (code, stack) with
  | PUSH v :: ds, stack -> advance ds (V v :: stack)
  | POP :: ds, _ :: stack -> advance ds stack
  | SWAP :: ds, e1 :: e2 :: stack -> advance ds (e2 :: e1 :: stack)
  | BIND x :: ds, V v :: stack -> advance ds (EV [ (x, v) ] :: stack)
  | LOOKUP x :: ds, stack -> advance ds (V (search x stack) :: stack)
  | UNARY op :: ds, V v :: stack ->
      advance ds (V (Ast.Unary_op.to_fun op v) :: stack)
  | OPER op :: ds, V v2 :: V v1 :: stack ->
      advance ds (V (Ast.Binary_op.to_fun op (v1, v2)) :: stack)
  | MK_PAIR :: ds, V v2 :: V v1 :: stack ->
      advance ds (V (Pair (v1, v2)) :: stack)
  | FST :: ds, V (Pair (v, _)) :: stack -> advance ds (V v :: stack)
  | SND :: ds, V (Pair (_, v)) :: stack -> advance ds (V v :: stack)
  | MK_INL :: ds, V v :: stack -> advance ds (V (Inl v) :: stack)
  | MK_INR :: ds, V v :: stack -> advance ds (V (Inr v) :: stack)
  | CASE (c1, _) :: ds, V (Inl v) :: stack -> advance (c1 @ ds) (V v :: stack)
  | CASE (_, c2) :: ds, V (Inr v) :: stack -> advance (c2 @ ds) (V v :: stack)
  | TEST (c1, _) :: ds, V (Bool true) :: stack -> advance (c1 @ ds) stack
  | TEST (_, c2) :: ds, V (Bool false) :: stack -> advance (c2 @ ds) stack
  | ASSIGN :: ds, V v :: V (Ref a) :: stack ->
      { code = ds; stack = V Unit :: stack; heap = Heap.set a v state.heap }
  | DEREF :: ds, V (Ref a) :: stack -> advance ds (V (Heap.get a heap) :: stack)
  | MK_REF :: ds, V v :: stack ->
      let a, heap = Heap.alloc v heap in
      { code = ds; stack = V (Ref a) :: stack; heap }
  | WHILE (_, _) :: ds, V (Bool false) :: stack -> advance ds (V Unit :: stack)
  | WHILE (cond, body) :: ds, V (Bool true) :: stack ->
      advance (body @ [ POP ] @ cond @ [ WHILE (cond, body) ] @ ds) stack
  | MK_CLOSURE c :: ds, stack ->
      advance ds (V (Fun (Closure (c, evs_to_env stack))) :: stack)
  | MK_REC (f, c) :: ds, stack ->
      advance ds (V (Fun (Rec_closure (f, (c, evs_to_env stack)))) :: stack)
  | APPLY :: ds, V (Fun (Closure (c, env))) :: V v :: stack ->
      advance (c @ ds) (V v :: EV env :: stack)
  | APPLY :: ds, V (Fun (Rec_closure (f, (c, env)))) :: V v :: stack ->
      advance (APPLY :: ds)
        (V (Fun (Closure (c, (f, Fun (Rec_closure (f, (c, env)))) :: env)))
        :: V v :: stack)
  | _ -> Errors.complainf "Step: bad state = %a" pp_state state

let rec driver n state =
  if Option.verbose then
    Format.printf "State %d: %a@." n pp_state state;

  match (state.code, state.stack) with
  | [], [ V v ] -> v
  | _ -> driver (n + 1) (step state)

(* A BIND will leave an env on stack.
   This gets rid of it.  *)
let leave_scope = [ SWAP; POP ]

let rec compile : Ast.t -> code = function
  | Unit -> [ PUSH Unit ]
  | Integer n -> [ PUSH (Int n) ]
  | Boolean b -> [ PUSH (Bool b) ]
  | Var x -> [ LOOKUP x ]
  | UnaryOp (op, e) -> compile e @ [ UNARY op ]
  | BinaryOp (e1, op, e2) -> compile e1 @ compile e2 @ [ OPER op ]
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

let interpret (e : Ast.t) : value =
  let c = compile e in

  if Option.verbose then
    Format.printf "Compile code =@\n%a@." pp_code c;

  driver 1 { code = c; stack = []; heap = Heap.empty }
