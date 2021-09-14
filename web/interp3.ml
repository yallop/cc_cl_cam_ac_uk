open Js_of_ocaml
open Slang
open Interp_3
open Ast

let int_string_list_string_list_list_of_state (cp, evs, heap_list) = (cp, List.map Interp_3.string_of_env_or_value evs, List.map Interp_3.string_of_value heap_list)

(* Generate strings of code with location data for the frontend *)

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
let list_of_heap _ = Array.to_list (Array.sub Interp_3.heap 0 (!Interp_3.next_address))

let drop_tag_of_code c = List.map (Interp_3.map (fun _ -> ())) c

let rec driver (cp, env) = let heapl = list_of_heap() in (cp, env, heapl) ::
 if Interp_3.HALT () = Interp_3.map (fun _ -> ()) @@ Interp_3.get_instruction cp
    then []
    else driver (Interp_3.step (cp, env)) 

let stacks e =
  let c = drop_tag_of_code @@ Interp_3.compile e in
  let _ = Interp_3.installed := Interp_3.load c in 
  let installed_code = Interp_3.string_of_installed_code() in
  (installed_code, List.map int_string_list_string_list_list_of_state (driver (0, [])))



(* Export a representation of each interpreter step in a stream *)

let rec nsteps states n = match (states, n) with
  | ([], _) -> states 
  | (_, 0) -> states
  | ((cp, env, _)::_, n) -> if Interp_3.HALT () != Interp_3.map (fun _ -> ()) @@ Interp_3.get_instruction cp
      then let (cp', env') = Interp_3.step (cp, env) in
        let heapl = list_of_heap() in nsteps ((cp', env', heapl) :: states) (n - 1) else states

let js_string_of_states states = Js.string @@ Yojson.Safe.to_string @@ [%yojson_of: (int * string list * string list) list] @@ List.map int_string_list_string_list_list_of_state states

let rec streamDriver' states n =
  let new_states = nsteps states n in
  (object%js
    val steps = js_string_of_states new_states
    method next = streamDriver' new_states n
  end)

let streamDriver e n =
  let c = Interp_3.compile e in
  let _ = Interp_3.installed := Interp_3.load (drop_tag_of_code c) in
  (object%js
    val code = Js.string @@ Yojson.Safe.to_string @@ [%yojson_of: (int * string) list] @@ loc_string_list_of_code c
    val stepStream = streamDriver' [(0, [], [])] n
  end)
 

