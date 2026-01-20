(**************************************
Compiler Construction 2020
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)
(*
   Interpreter 3.

   Derived from Interpreter 2 by
   --- Make instructions linear by introducing
       labels and jumps.
   --- labels translated to numeric addresses.
   --- include "code pointer" in state
   --- compiler elimnates WHILE construct
*)

open Ast
open Errors

type address = int
type label = string
type location = label * address option

type value =
  | REF of address
  | INT of int
  | BOOL of bool
  | UNIT
  | PAIR of value * value
  | INL of value
  | INR of value
  | CLOSURE of location * env
  | REC_CLOSURE of location

and instruction =
  | PUSH of value
  | LOOKUP of Ast.var
  | UNARY of Ast.unary_oper
  | OPER of Ast.oper
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

and code = instruction list
and binding = var * value
and env = binding list

type env_or_value =
  | EV of env (* an environment on the run-time stack *)
  | V of value (* a value on the run-time stack *)
  | RA of address (* a return address on the run-time stack *)

type env_value_stack = env_or_value list
type state = address * env_value_stack

(* update : (env * binding) -> env *)
(* let update(env, (x, v)) = (x, v) :: env *)

let rec lookup (env, x) =
  match env with
  | [] -> None
  | (y, v) :: rest ->
      if x = y then
        Some
          (match v with
          | REC_CLOSURE loc -> CLOSURE (loc, (y, REC_CLOSURE loc) :: rest)
          | _ -> v)
      else
        lookup (rest, x)

let rec search (evs, x) =
  match evs with
  | [] -> complainf "%s is not defined!@\n" x
  | V _ :: rest -> search (rest, x)
  | RA _ :: rest -> search (rest, x)
  | EV env :: rest -> (
      match lookup (env, x) with None -> search (rest, x) | Some v -> v)

let rec evs_to_env = function
  | [] -> []
  | V _ :: rest -> evs_to_env rest
  | RA _ :: rest -> evs_to_env rest
  | EV env :: rest -> env @ evs_to_env rest

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
  | PAIR (v1, v2) -> pr fmt "(@[%a,@ %a)@]" pp_value v1 pp_value v2
  | INL v -> pr fmt "inl(%a)" pp_value v
  | INR v -> pr fmt "inr(%a)" pp_value v
  | CLOSURE (loc, c) -> pr fmt "CLOSURE(%a)" pp_closure (loc, c)
  | REC_CLOSURE loc -> pr fmt "REC_CLOSURE(%a)" pp_location loc

and pp_closure fmt (loc, env) = pr fmt "(%a, %a)" pp_location loc pp_env env
and pp_env fmt env = pp_list fmt ",@\n " pp_binding env
and pp_binding fmt (x, v) = pr fmt "(%s, %a)" x pp_value v

and pp_location fmt = function
  | l, None -> pr fmt "%s" l
  | l, Some i -> pr fmt "%s = %d" l i

and pp_instruction fmt = function
  | UNARY op -> pr fmt "  UNARY %s" (string_of_uop op)
  | OPER op -> pr fmt "  OPER %s" (string_of_bop op)
  | MK_PAIR -> pr fmt "  MK_PAIR"
  | FST -> pr fmt "  FST"
  | SND -> pr fmt "  SND"
  | MK_INL -> pr fmt "  MK_INL"
  | MK_INR -> pr fmt "  MK_INR"
  | MK_REF -> pr fmt "  MK_REF"
  | PUSH v -> pr fmt "  PUSH %a" pp_value v
  | LOOKUP x -> pr fmt "  LOOKUP %s" x
  | TEST label -> pr fmt "  TEST %a" pp_location label
  | CASE label -> pr fmt "  CASE %a" pp_location label
  | GOTO label -> pr fmt "  GOTO %a" pp_location label
  | APPLY -> pr fmt "  APPLY"
  | RETURN -> pr fmt "  RETURN"
  | HALT -> pr fmt "  HALT"
  | BIND x -> pr fmt "  BIND %s" x
  | LABEL label -> pr fmt "LABEL %s:" label
  | SWAP -> pr fmt "  SWAP"
  | POP -> pr fmt "  POP"
  | DEREF -> pr fmt "  DEREF"
  | ASSIGN -> pr fmt "  ASSIGN"
  | MK_CLOSURE loc -> pr fmt "  MK_CLOSURE(%a)" pp_location loc
  | MK_REC (v, loc) -> pr fmt "  MK_REC(@[%s, %a)@]" v pp_location loc

and pp_code fmt c = List.iter (pr fmt "%a@\n" pp_instruction) c

let pp_env_or_value fmt = function
  | EV env -> pr fmt "EV %a" pp_env env
  | V v -> pr fmt "V %a" pp_value v
  | RA i -> pr fmt "RA %d" i

let pp_env_value_stack fmt = pp_list fmt ";@\n " pp_env_or_value
let string_of_value = Format.asprintf "%a" pp_value
let string_of_location = Format.asprintf "%a" pp_location
let string_of_code = Format.asprintf "%a" pp_code
let string_of_env_or_value = Format.asprintf "%a" pp_env_or_value

(* THE MACHINE *)

let installed = ref (Array.of_list [ HALT ])

let pp_installed_code fmt =
  let size = Array.length !installed in
  let rec aux fmt k =
    if size <> k then
      pr fmt "%d: %a@\n%a" k pp_instruction !installed.(k) aux (k + 1)
  in
  aux fmt 0

let string_of_installed_code () =
  Format.asprintf "%a" (fun f () -> pp_installed_code f) ()

let get_instruction cp = Array.get !installed cp
let heap = Array.make Option.heap_max (INT 0)
let next_address = ref 0

let new_address () =
  let a = !next_address in
  next_address := a + 1;
  a

let string_of_heap () =
  let rec aux k =
    if !next_address < k then
      ""
    else
      string_of_int k ^ " -> " ^ string_of_value heap.(k) ^ "\n" ^ aux (k + 1)
  in
  "\nHeap = \n" ^ aux 0

let pp_state fmt (cp, evs) =
  pr fmt "@\nCode Pointer = %d -> %a@\nStack = %a%s@\n" cp pp_instruction
    (get_instruction cp) pp_env_value_stack evs
    (if !next_address = 0 then
       ""
     else
       string_of_heap ())

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

let step (cp, evs) =
  match (get_instruction cp, evs) with
  | PUSH v, evs -> (cp + 1, V v :: evs)
  | POP, _ :: evs -> (cp + 1, evs)
  | SWAP, s1 :: s2 :: evs -> (cp + 1, s2 :: s1 :: evs)
  | BIND x, V v :: evs -> (cp + 1, EV [ (x, v) ] :: evs)
  | LOOKUP x, evs -> (cp + 1, V (search (evs, x)) :: evs)
  | UNARY op, V v :: evs -> (cp + 1, V (do_unary (op, v)) :: evs)
  | OPER op, V v2 :: V v1 :: evs -> (cp + 1, V (do_oper (op, v1, v2)) :: evs)
  | MK_PAIR, V v2 :: V v1 :: evs -> (cp + 1, V (PAIR (v1, v2)) :: evs)
  | FST, V (PAIR (v, _)) :: evs -> (cp + 1, V v :: evs)
  | SND, V (PAIR (_, v)) :: evs -> (cp + 1, V v :: evs)
  | MK_INL, V v :: evs -> (cp + 1, V (INL v) :: evs)
  | MK_INR, V v :: evs -> (cp + 1, V (INR v) :: evs)
  | CASE (_, Some _), V (INL v) :: evs -> (cp + 1, V v :: evs)
  | CASE (_, Some i), V (INR v) :: evs -> (i, V v :: evs)
  | TEST (_, Some _), V (BOOL true) :: evs -> (cp + 1, evs)
  | TEST (_, Some i), V (BOOL false) :: evs -> (i, evs)
  | ASSIGN, V v :: V (REF a) :: evs ->
      heap.(a) <- v;
      (cp + 1, V UNIT :: evs)
  | DEREF, V (REF a) :: evs -> (cp + 1, V heap.(a) :: evs)
  | MK_REF, V v :: evs ->
      let a = new_address () in
      heap.(a) <- v;
      (cp + 1, V (REF a) :: evs)
  | MK_CLOSURE loc, evs -> (cp + 1, V (CLOSURE (loc, evs_to_env evs)) :: evs)
  | MK_REC (f, loc), evs ->
      (cp + 1, V (CLOSURE (loc, (f, REC_CLOSURE loc) :: evs_to_env evs)) :: evs)
  | APPLY, V (CLOSURE ((_, Some i), env)) :: V v :: evs ->
      (i, V v :: EV env :: RA (cp + 1) :: evs)
  (* new intructions *)
  | RETURN, V v :: _ :: RA i :: evs -> (i, V v :: evs)
  | LABEL _, evs -> (cp + 1, evs)
  | HALT, evs -> (cp, evs)
  | GOTO (_, Some i), evs -> (i, evs)
  | _ -> complainf "step : bad state = %a\n" pp_state (cp, evs)

(* COMPILE *)

let label_ref = ref 0

let new_label =
  let get () =
    let v = !label_ref in
    label_ref := !label_ref + 1;
    "L" ^ string_of_int v
  in
  get

let rec comp = function
  | Unit -> ([], [ PUSH UNIT ])
  | Integer n -> ([], [ PUSH (INT n) ])
  | Boolean b -> ([], [ PUSH (BOOL b) ])
  | Var x -> ([], [ LOOKUP x ])
  | UnaryOp (op, e) ->
      let defs, c = comp e in
      (defs, c @ [ UNARY op ])
  | Op (e1, op, e2) ->
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
        @ [ POP; GOTO (test_label, None); LABEL end_label; PUSH UNIT ] )
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

let compile e =
  let defs, c = comp e in
  (* The HALT instruction needs an annotation to satisfy the types
     We arbitarily use the annotation from the root of the AST
   *)
  let result =
    c (* body of program *) @ [ HALT ] (* stop the interpreter *) @ defs
  in
  (* the function definitions *)
  let () =
    if Option.verbose then
      Format.printf "@\nCompiled Code = @\n%a" pp_code result
  in
  result

let rec driver n ((cp, evs) as state) =
  if Option.verbose then
    Format.printf "\nstate %d:%a\n" n pp_state state;
  match (get_instruction cp, evs) with
  | HALT, [ V v ] -> v
  | HALT, _ -> complainf "driver : bad halted state = %a\n" pp_state state
  | _ -> driver (n + 1) (step state)

(* put code listing into an array, associate an array index to each label *)
let load l =
  let rec find lab = function
    | [] -> complainf "find : %s is not found" lab
    | (x, v) :: rest ->
        if x = lab then
          v
        else
          find lab rest
    (* insert array index for each label *)
  in
  let apply_label_map_to_instruction m = function
    | GOTO (lab, _) -> GOTO (lab, Some (find lab m))
    | TEST (lab, _) -> TEST (lab, Some (find lab m))
    | CASE (lab, _) -> CASE (lab, Some (find lab m))
    | MK_CLOSURE (lab, _) -> MK_CLOSURE (lab, Some (find lab m))
    | MK_REC (f, (lab, _)) -> MK_REC (f, (lab, Some (find lab m)))
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

(* interpret : expr -> value *)
let interpret e =
  let c = compile e in
  let () = installed := load c in
  let () =
    if Option.verbose then
      Format.printf "\nInstalled Code = \n%s" (string_of_installed_code ())
  in
  (* set the code pointer to 0 *)
  driver 1 (0, [])

let reset =
 fun () ->
  next_address := 0;
  label_ref := 0;
  Array.fill heap 0 (Array.length heap) (INT 0)
