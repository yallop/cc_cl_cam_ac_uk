open Slanglib
open Interp_3
open Ast

let string_state (cp, evs, heap_list) =
  ( cp,
    List.map Interp_3.string_of_env_or_value evs,
    List.map Interp_3.string_of_value heap_list )

let list_of_heap _ =
  Array.to_list (Array.sub Interp_3.heap 0 !Interp_3.next_address)

let rec driver n (cp, env) =
  let heapl = list_of_heap () in
  (cp, env, heapl)
  ::
  (if Interp_3.HALT = Interp_3.get_instruction cp then
     []
   else
     driver (n + 1) (Interp_3.step (cp, env)))

let stacks e =
  let c = Interp_3.compile e in
  let _ = Interp_3.installed := Interp_3.load c in
  let installed_code = Interp_3.string_of_installed_code () in
  (installed_code, List.map string_state (driver 1 (0, [])))

let string_list_of_instruction : instruction -> string list = function
  | UNARY op -> [ "UNARY " ^ string_of_uop op ]
  | OPER op -> [ "OPER " ^ string_of_bop op ]
  | MK_PAIR -> [ "MK_PAIR" ]
  | FST -> [ "FST" ]
  | SND -> [ "SND" ]
  | MK_INL -> [ "MK_INL" ]
  | MK_INR -> [ "MK_INR" ]
  | MK_REF -> [ "MK_REF" ]
  | PUSH v -> [ "PUSH " ^ string_of_value v ]
  | LOOKUP x -> [ "LOOKUP " ^ x ]
  | TEST label -> [ "TEST " ^ string_of_location label ]
  | CASE label -> [ "CASE " ^ string_of_location label ]
  | GOTO label -> [ "GOTO " ^ string_of_location label ]
  | APPLY -> [ "APPLY" ]
  | RETURN -> [ "RETURN" ]
  | HALT -> [ "HALT" ]
  | BIND x -> [ "BIND " ^ x ]
  | LABEL label -> [ "LABEL " ^ label ]
  | SWAP -> [ "SWAP" ]
  | POP -> [ "POP" ]
  | DEREF -> [ "DEREF" ]
  | ASSIGN -> [ "ASSIGN" ]
  | MK_CLOSURE loc -> [ "MK_CLOSURE(" ^ string_of_location loc ^ ")" ]
  | MK_REC (v, loc) -> [ "MK_REC(" ^ v ^ ", " ^ string_of_location loc ^ ")" ]

let string_list_of_code c =
  List.flatten @@ List.map string_list_of_instruction c
