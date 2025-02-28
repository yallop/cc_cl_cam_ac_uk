open Slanglib
open Interp_3
open Ast

let string_state (cp, evs, heap_list) = (cp, List.map Interp_3.string_of_env_or_value evs, List.map Interp_3.string_of_value heap_list)

let list_of_heap _ = Array.to_list (Array.sub Interp_3.heap 0 (!Interp_3.next_address))

let drop_tag_of_code c = List.map (Interp_3.map (fun _ -> ())) c

let rec driver n (cp, env) = let heapl = list_of_heap() in (cp, env, heapl) ::
 if Interp_3.HALT () = Interp_3.map (fun _ -> ()) @@ Interp_3.get_instruction cp
    then []
    else driver (n + 1) (Interp_3.step (cp, env)) 

let stacks e =
  let c = drop_tag_of_code @@ Interp_3.compile e in
  let _ = Interp_3.installed := Interp_3.load c in 
  let installed_code = Interp_3.string_of_installed_code() in
  (installed_code, List.map string_state (driver 1 (0, [])))

let  loc_string_list_of_instruction : Past.loc instruction -> (int * string) list = function
  | UNARY({pos_lnum = lnum; _}, op) -> [(lnum, "UNARY " ^ (string_of_uop op))]
  | OPER({pos_lnum = lnum; _}, op)  -> [(lnum, "OPER " ^ (string_of_bop op))]
  | MK_PAIR {pos_lnum = lnum; _}   -> [(lnum, "MK_PAIR")]
  | FST {pos_lnum = lnum; _}    -> [(lnum, "FST")]
  | SND {pos_lnum = lnum; _}    -> [(lnum, "SND")]
  | MK_INL {pos_lnum = lnum; _} -> [(lnum, "MK_INL")]
  | MK_INR {pos_lnum = lnum; _} -> [(lnum, "MK_INR")]
  | MK_REF {pos_lnum = lnum; _} -> [(lnum, "MK_REF")]
  | PUSH({pos_lnum = lnum; _}, v)   -> [(lnum, "PUSH " ^ (string_of_value v))]
  | LOOKUP({pos_lnum = lnum; _}, x) -> [(lnum, "LOOKUP " ^ x)]
  | TEST({pos_lnum = lnum; _}, label)   -> [(lnum, "TEST " ^ (string_of_location label))]
  | CASE({pos_lnum = lnum; _}, label)   -> [(lnum, "CASE " ^ (string_of_location label))]
  | GOTO({pos_lnum = lnum; _}, label)   -> [(lnum, "GOTO " ^ (string_of_location label))]
  | APPLY {pos_lnum = lnum; _}  -> [(lnum, "APPLY")]
  | RETURN {pos_lnum = lnum; _} -> [(lnum, "RETURN")]
  | HALT {pos_lnum = lnum; _}   -> [(lnum, "HALT")]
  | BIND({pos_lnum = lnum; _}, x)   -> [(lnum, "BIND " ^ x)]
  | LABEL({pos_lnum = lnum; _}, label)  -> [(lnum, "LABEL " ^ label)]
  | SWAP {pos_lnum = lnum; _}   -> [(lnum, "SWAP")]
  | POP {pos_lnum = lnum; _}    -> [(lnum, "POP")]
  | DEREF {pos_lnum = lnum; _}  -> [(lnum, "DEREF")]
  | ASSIGN {pos_lnum = lnum; _} -> [(lnum, "ASSIGN")]
  | MK_CLOSURE({pos_lnum = lnum; _}, loc)  -> [(lnum, "MK_CLOSURE(" ^ (string_of_location loc) ^ ")")]
  | MK_REC({pos_lnum = lnum; _}, v, loc) -> [(lnum, "MK_REC(" ^ v ^ ", " ^ (string_of_location loc) ^ ")")]

let loc_string_list_of_code c =  List.flatten @@ List.map loc_string_list_of_instruction c
