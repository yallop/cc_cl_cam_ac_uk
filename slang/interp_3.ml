(**************************************
Compiler Construction 2020
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

(** Interpreter 3.

    Derived from Interpreter 2 by

    --- Make instructions linear by introducing labels and jumps.

    --- labels translated to numeric addresses.

    --- include "code pointer" in state

    --- compiler elimnates WHILE construct *)

type address = int [@@deriving show]
type label = string [@@deriving show]
type location = label * address option [@@deriving show]

type value = f Value.t
and f = Closure of location * env | Rec_closure of location

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
  | RETURN
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of location
  | MK_REC of Ast.var * location
  | TEST of location
  | CASE of location
  | GOTO of location
  | LABEL of label
  | HALT

and binding = Ast.var * value
and env = binding list [@@deriving show { with_path = false }]

type env_or_value =
  | EV of env (* an environment on the run-time stack *)
  | V of value (* a value on the run-time stack *)
  | RA of address (* a return address on the run-time stack *)
[@@deriving show { with_path = false }]

type env_value_stack = env_or_value list [@@deriving show { with_path = false }]
type state = { heap : value Heap.t; cp : int; stack : env_value_stack }

let init_state = { heap = Heap.empty; cp = 0; stack = [] }

let rec lookup x : env -> value option = function
  | [] -> None
  | (y, v) :: rest when x = y ->
      Some
        (match v with
        | Fun (Rec_closure loc) ->
            Fun (Closure (loc, (y, Fun (Rec_closure loc)) :: rest))
        | _ -> v)
  | _ :: rest -> lookup x rest

let rec search x : env_value_stack -> value = function
  | [] -> Errors.complainf "%s is not defined!@\n" x
  | V _ :: rest | RA _ :: rest -> search x rest
  | EV env :: rest -> (
      match lookup x env with None -> search x rest | Some v -> v)

let rec evs_to_env = function
  | [] -> []
  | V _ :: rest -> evs_to_env rest
  | RA _ :: rest -> evs_to_env rest
  | EV env :: rest -> env @ evs_to_env rest

let rec pp_value ppf : value -> unit = Value.pp pp_fun ppf

and pp_fun ppf = function
  | Closure (loc, env) ->
      Format.fprintf ppf "(%a, %a)" pp_location loc pp_env env
  | Rec_closure _ -> Format.fprintf ppf ""

and pp_location ppf = function
  | l, None -> Format.fprintf ppf "%s" l
  | l, Some i -> Format.fprintf ppf "%s = %d" l i

and pp_instruction ppf = function
  | UNARY op -> Format.fprintf ppf "  UNARY %a" Ast.Unary_op.pp op
  | OPER op -> Format.fprintf ppf "  OPER %a" Ast.Binary_op.pp op
  | MK_PAIR -> Format.fprintf ppf "  MK_PAIR"
  | FST -> Format.fprintf ppf "  FST"
  | SND -> Format.fprintf ppf "  SND"
  | MK_INL -> Format.fprintf ppf "  MK_INL"
  | MK_INR -> Format.fprintf ppf "  MK_INR"
  | MK_REF -> Format.fprintf ppf "  MK_REF"
  | PUSH v -> Format.fprintf ppf "  PUSH %a" pp_value v
  | LOOKUP x -> Format.fprintf ppf "  LOOKUP %s" x
  | TEST label -> Format.fprintf ppf "  TEST %a" pp_location label
  | CASE label -> Format.fprintf ppf "  CASE %a" pp_location label
  | GOTO label -> Format.fprintf ppf "  GOTO %a" pp_location label
  | APPLY -> Format.fprintf ppf "  APPLY"
  | RETURN -> Format.fprintf ppf "  RETURN"
  | HALT -> Format.fprintf ppf "  HALT"
  | BIND x -> Format.fprintf ppf "  BIND %s" x
  | LABEL label -> Format.fprintf ppf "LABEL %s:" label
  | SWAP -> Format.fprintf ppf "  SWAP"
  | POP -> Format.fprintf ppf "  POP"
  | DEREF -> Format.fprintf ppf "  DEREF"
  | ASSIGN -> Format.fprintf ppf "  ASSIGN"
  | MK_CLOSURE loc -> Format.fprintf ppf "  MK_CLOSURE(%a)" pp_location loc
  | MK_REC (v, loc) ->
      Format.fprintf ppf "  MK_REC(@[%s, %a)@]" v pp_location loc

and pp_code ppf code =
  let annotated_code = List.mapi (fun i c -> (i, c)) code in
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "@.")
    (fun ppf (i, c) -> Format.fprintf ppf "%d: %a" i pp_instruction c)
    ppf annotated_code

(* THE MACHINE *)

let pp_state code ppf { cp; stack; heap } =
  Format.fprintf ppf "Code Pointer = %d -> %a@.Stack = %a@.Heap = %a@." cp
    pp_instruction code.(cp) pp_env_value_stack stack (Heap.pp pp_value) heap

let step code ({ cp; stack; heap } as state) : state =
  let advance = { state with cp = cp + 1 } in
  match (code.(cp), stack) with
  | PUSH v, evs -> { advance with stack = V v :: evs }
  | POP, _ :: evs -> { advance with stack = evs }
  | SWAP, s1 :: s2 :: evs -> { state with cp = cp + 1; stack = s2 :: s1 :: evs }
  | BIND x, V v :: evs -> { advance with stack = EV [ (x, v) ] :: evs }
  | LOOKUP x, evs -> { advance with stack = V (search x evs) :: evs }
  | UNARY op, V v :: evs ->
      { state with cp = cp + 1; stack = V (Ast.Unary_op.to_fun op v) :: evs }
  | OPER op, V v2 :: V v1 :: evs ->
      { advance with stack = V (Ast.Binary_op.to_fun op (v1, v2)) :: evs }
  | MK_PAIR, V v2 :: V v1 :: evs ->
      { advance with stack = V (Pair (v1, v2)) :: evs }
  | FST, V (Pair (v, _)) :: evs -> { advance with stack = V v :: evs }
  | SND, V (Pair (_, v)) :: evs -> { advance with stack = V v :: evs }
  | MK_INL, V v :: evs -> { advance with stack = V (Inl v) :: evs }
  | MK_INR, V v :: evs -> { advance with stack = V (Inr v) :: evs }
  | CASE (_, Some _), V (Inl v) :: evs -> { advance with stack = V v :: evs }
  | CASE (_, Some i), V (Inr v) :: evs ->
      { advance with cp = i; stack = V v :: evs }
  | TEST (_, Some _), V (Bool true) :: evs -> { advance with stack = evs }
  | TEST (_, Some i), V (Bool false) :: evs ->
      { advance with cp = i; stack = evs }
  | ASSIGN, V v :: V (Ref a) :: evs ->
      { advance with stack = V Unit :: evs; heap = Heap.set a v heap }
  | DEREF, V (Ref a) :: evs ->
      { advance with stack = V (Heap.get a heap) :: evs }
  | MK_REF, V v :: evs ->
      let a, heap = Heap.alloc v heap in
      { advance with stack = V (Ref a) :: evs; heap }
  | MK_CLOSURE loc, evs ->
      { advance with stack = V (Fun (Closure (loc, evs_to_env evs))) :: evs }
  | MK_REC (f, loc), evs ->
      {
        advance with
        stack =
          V (Fun (Closure (loc, (f, Fun (Rec_closure loc)) :: evs_to_env evs)))
          :: evs;
      }
  | APPLY, V (Fun (Closure ((_, Some i), env))) :: V v :: evs ->
      { state with cp = i; stack = V v :: EV env :: RA (cp + 1) :: evs }
  (* new instructions *)
  | RETURN, V v :: _ :: RA i :: evs -> { state with cp = i; stack = V v :: evs }
  | LABEL _, _ -> advance
  | HALT, _ -> state
  | GOTO (_, Some i), _ -> { state with cp = i }
  | _ -> Errors.complainf "step : bad state = %a\n" (pp_state code) state

(* COMPILE *)

let compile e =
  let label_ref = ref 0 in

  let new_label () =
    let v = !label_ref in
    label_ref := !label_ref + 1;
    Format.sprintf "L%d" v
  in

  let rec comp : Ast.t -> instruction list * instruction list = function
    | Unit -> ([], [ PUSH Unit ])
    | Integer n -> ([], [ PUSH (Int n) ])
    | Boolean b -> ([], [ PUSH (Bool b) ])
    | Var x -> ([], [ LOOKUP x ])
    | UnaryOp (op, e) ->
        let defs, c = comp e in
        (defs, c @ [ UNARY op ])
    | BinaryOp (e1, op, e2) ->
        let defs1, c1 = comp e1 in
        let defs2, c2 = comp e2 in
        (defs1 @ defs2, c1 @ c2 @ [ OPER op ])
    | Pair (e1, e2) ->
        let defs1, c1 = comp e1 in
        let defs2, c2 = comp e2 in
        (defs1 @ defs2, c1 @ c2 @ [ MK_PAIR ])
    | Fst e ->
        let defs, c = comp e in
        (defs, c @ [ FST ])
    | Snd e ->
        let defs, c = comp e in
        (defs, c @ [ SND ])
    | Inl e ->
        let defs, c = comp e in
        (defs, c @ [ MK_INL ])
    | Inr e ->
        let defs, c = comp e in
        (defs, c @ [ MK_INR ])
    | Case (e1, (x1, e2), (x2, e3)) ->
        let inr_label = new_label () in
        let after_inr_label = new_label () in
        let defs1, c1 = comp e1 in
        let defs2, c2 = comp e2 in
        let defs3, c3 = comp e3 in
        ( defs1 @ defs2 @ defs3,
          c1
          @ [ CASE (inr_label, None) ]
          @ ((BIND x1 :: c2) @ [ SWAP; POP ])
          @ [ GOTO (after_inr_label, None); LABEL inr_label ]
          @ ((BIND x2 :: c3) @ [ SWAP; POP ])
          @ [ LABEL after_inr_label ] )
    | If (e1, e2, e3) ->
        let else_label = new_label () in
        let after_else_label = new_label () in
        let defs1, c1 = comp e1 in
        let defs2, c2 = comp e2 in
        let defs3, c3 = comp e3 in
        ( defs1 @ defs2 @ defs3,
          c1
          @ [ TEST (else_label, None) ]
          @ c2
          @ [ GOTO (after_else_label, None); LABEL else_label ]
          @ c3 @ [ LABEL after_else_label ] )
    | Seq [] -> ([], [])
    | Seq [ e ] -> comp e
    | Seq (e :: rest) ->
        let defs1, c1 = comp e in
        let defs2, c2 = comp (Seq rest) in
        (defs1 @ defs2, c1 @ [ POP ] @ c2)
    | Ref e ->
        let defs, c = comp e in
        (defs, c @ [ MK_REF ])
    | Deref e ->
        let defs, c = comp e in
        (defs, c @ [ DEREF ])
    | While (e1, e2) ->
        let test_label = new_label () in
        let end_label = new_label () in
        let defs1, c1 = comp e1 in
        let defs2, c2 = comp e2 in
        ( defs1 @ defs2,
          [ LABEL test_label ] @ c1
          @ [ TEST (end_label, None) ]
          @ c2
          @ [ POP; GOTO (test_label, None); LABEL end_label; PUSH Unit ] )
    | Assign (e1, e2) ->
        let defs1, c1 = comp e1 in
        let defs2, c2 = comp e2 in
        (defs1 @ defs2, c1 @ c2 @ [ ASSIGN ])
    | App (e1, e2) ->
        let defs1, c1 = comp e1 in
        let defs2, c2 = comp e2 in
        (defs1 @ defs2, c2 @ c1 @ [ APPLY ])
    | Lambda (x, e) ->
        let defs, c = comp e in
        let f = new_label () in
        let def = [ LABEL f; BIND x ] @ c @ [ SWAP; POP; RETURN ] in
        (def @ defs, [ MK_CLOSURE (f, None) ])
    (*
 Note that we could have

 | LetFun(f, (x, e1), e2) -> comp (App(Lambda(f, e2), Lambda(x, e1)))

 This would then result (ignoring the defs generated by subterms) in

    defs = [LABEL g ; BIND f] @ c2 @ [SWAP; POP; RETURN]
         @ [LABEL h; bind x ] @ c1 @ [SWAP; POP; RETURN]

    code = [MK_CLOSURE((h, None)); [MK_CLOSURE((g, None)); APPLY]

  where g and h are new labels.

  In contrast, the following version of comp results in

     defs = [LABEL f; BIND x] @ c1 @ [SWAP; POP; RETURN]

     code = [MK_CLOSURE((f, None)); BIND f] @ c2 @ [SWAP; POP])

  which is simpler.

*)
    | LetFun (f, (x, e1), e2) ->
        let defs1, c1 = comp e1 in
        let defs2, c2 = comp e2 in
        let lab = new_label () in
        let def = [ LABEL lab; BIND x ] @ c1 @ [ SWAP; POP; RETURN ] in
        ( def @ defs1 @ defs2,
          [ MK_CLOSURE (lab, None); BIND f ] @ c2 @ [ SWAP; POP ] )
    | LetRecFun (f, (x, e1), e2) ->
        let defs1, c1 = comp e1 in
        let defs2, c2 = comp e2 in
        let lab = new_label () in
        let def = [ LABEL lab; BIND x ] @ c1 @ [ SWAP; POP; RETURN ] in
        ( def @ defs1 @ defs2,
          [ MK_REC (f, (lab, None)); BIND f ] @ c2 @ [ SWAP; POP ] )
  in
  let defs, c = comp e in
  (* The HALT instruction needs an annotation to satisfy the types
     We arbitarily use the annotation from the root of the AST
   *)
  let result =
    (* Body of program @ stop the interpreter @ function definitions *)
    c @ (HALT :: defs)
  in

  if Option.verbose then
    Format.printf "Compiled Code = %a@." pp_code result;
  result

(** Put code listing into an array, associate an array index to each label *)
let load l =
  (* Insert array index for each label *)
  let apply_label_map_to_instruction m = function
    | GOTO (lab, _) -> GOTO (lab, Some (List.assoc lab m))
    | TEST (lab, _) -> TEST (lab, Some (List.assoc lab m))
    | CASE (lab, _) -> CASE (lab, Some (List.assoc lab m))
    | MK_CLOSURE (lab, _) -> MK_CLOSURE (lab, Some (List.assoc lab m))
    | MK_REC (f, (lab, _)) -> MK_REC (f, (lab, Some (List.assoc lab m)))
    | inst -> inst
  in
  (* find array index for each label *)
  let listing_to_label_map l =
    let rec aux carry k = function
      | [] -> carry
      | LABEL lab :: rest -> aux ((lab, k) :: carry) (k + 1) rest
      | _ :: rest -> aux carry (k + 1) rest
    in
    aux [] 0 l
  in
  let l_map = listing_to_label_map l in
  Array.of_list (List.map (apply_label_map_to_instruction l_map) l)

let interpret (e : Ast.t) : value =
  let c = compile e in

  if Option.verbose then
    Format.printf "Compiled code = %a@." pp_code c;

  let c = load c in

  let rec driver (n : int) (code : instruction array) (state : state) : value =
    if Option.verbose then
      Format.printf "State %d: %a@\n" n (pp_state code) state;
    match (code.(state.cp), state.stack) with
    | HALT, [ V v ] -> v
    | HALT, _ ->
        Errors.complainf "Driver: bad halted state = %a@\n" (pp_state code)
          state
    | _ -> driver (n + 1) code (step code state)
  in

  driver 0 c init_state
