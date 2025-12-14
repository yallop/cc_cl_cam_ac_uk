(**************************************
Compiler Construction 2015
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

(** Interpreter 0 for Slang.2

    This is a "definitional" interpreter for for Slang.2 (the defined language)
    using high-level constructs of OCaml (the defining language). For examples,
    Slang.2 functions are represented as OCaml functions of type

    value -> value

    Slang conditionals are translated to OCaml conditionals, etc. The most
    interesting (and tricky) case is the "let rec" construct of Slang --- this
    is translated using the "lec rec" construct of OCaml. Not with the defined
    function itself, but with the definition of a recursive environment!
    (Because when a recursive function calls itself, it must find its own
    definition in the environment...)

    Note that some of the functions can fail. However, if the input expression
    has passed static analysis, then such "run time" errors should never happen!
    (Can you prove that?) *)

type value = f Value.t
and f = Run of (value -> store -> value * store)
and store = value Heap.t [@@deriving show { with_path = false }]

type env = Ast.var -> value

(* Auxiliary functions *)

let update (x, v) env =
 fun y ->
  if x = y then
    v
  else
    env y

let rec interp (e : Ast.t) (env : env) (store : store) : value * store =
  match e with
  | Unit -> (Unit, store)
  | Var x -> (env x, store)
  | Integer n -> (Int n, store)
  | Boolean b -> (Bool b, store)
  | Seq [] -> (Unit, store) (* should not be seen ... *)
  | Seq [ e ] -> interp e env store
  | Seq (e :: rest) ->
      let _, store = interp e env store in
      interp (Seq rest) env store
  | While (e1, e2) -> (
      let v, store = interp e1 env store in
      match v with
      | Bool true -> interp (Seq [ e2; e ]) env store
      | Bool false -> (Unit, store)
      | _ -> Errors.complain "Runtime error: expecting a boolean!")
  | Ref e ->
      let v, store = interp e env store in
      let a, store = Heap.alloc v store in
      (Ref a, store)
  | Deref e -> (
      let v, store = interp e env store in
      match v with
      | Ref a -> (Heap.get a store, store)
      | _ -> Errors.complain "Runtime error: expecting an address!")
  | Assign (e1, e2) -> (
      match interp e1 env store with
      | Ref a, store ->
          let v, store = interp e2 env store in
          (Unit, Heap.set a v store)
      | _ ->
          Errors.complain
            "Runtime error: expecting an address on left side of assignment")
  | UnaryOp (op, e) ->
      let v, store = interp e env store in
      (Ast.Unary_op.to_fun op v, store)
  | BinaryOp (e1, op, e2) ->
      let v1, store = interp e1 env store in
      let v2, store = interp e2 env store in
      (Ast.Binary_op.to_fun op (v1, v2), store)
  | If (e1, e2, e3) -> (
      let v, store = interp e1 env store in
      match v with
      | Bool true -> interp e2 env store
      | Bool false -> interp e3 env store
      | _ -> Errors.complain "Runtime error: expecting a boolean")
  | Pair (e1, e2) ->
      let v1, store = interp e1 env store in
      let v2, store = interp e2 env store in
      (Pair (v1, v2), store)
  | Fst e -> (
      match interp e env store with
      | Pair (v1, _), store -> (v1, store)
      | _ -> Errors.complain "Runtime error: expecting a pair")
  | Snd e -> (
      match interp e env store with
      | Pair (_, v2), store -> (v2, store)
      | _ -> Errors.complain "Runtime error: expecting a pair")
  | Inl e ->
      let v, store = interp e env store in
      (Inl v, store)
  | Inr e ->
      let v, store = interp e env store in
      (Inr v, store)
  | Case (e, (x1, e1), (x2, e2)) -> (
      let v, store = interp e env store in
      match v with
      | Inl v1 -> interp e1 (update (x1, v1) env) store
      | Inr v2 -> interp e2 (update (x2, v2) env) store
      | _ -> Errors.complain "Runtime error: expecting inl or inr")
  | Lambda (x, e) -> (Fun (Run (fun v -> interp e (update (x, v) env))), store)
  | App (e1, e2) -> (
      let v2, store = interp e2 env store in
      let v1, store = interp e1 env store in
      match v1 with
      | Fun (Run f) -> f v2 store
      | _ -> Errors.complain "Runtime error: expecting a function")
  | LetFun (f, (x, body), e) ->
      let env =
        update
          (f, Value.Fun (Run (fun v -> interp body (update (x, v) env))))
          env
      in
      interp e env store
  | LetRecFun (f, (x, body), e) ->
      (* A recursive environment! *)
      let rec new_env g : value =
        if g = f then
          Fun (Run (fun v -> interp body (update (x, v) new_env)))
        else
          env g
      in
      interp e new_env store

let interpret (e : Ast.t) : value =
  let empty_env : env = Errors.complainf "%s is not defined" in
  let v, _ = interp e empty_env Heap.empty in
  v
