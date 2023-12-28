open Slang
open Ast
open Interp_2

type 'a steps = 'a Interp_2.interp_state list

let initial_state c = (c, Interp_2.initial_env, Interp_2.initial_state)


(* Generate strings of code with location data for the frontend *)

let apply_to_last f l = let length = List.length l - 1 in List.mapi (fun i x -> if length = i then f x else x) l

let rec loc_string_list_of_code c =  match List.flatten @@ List.map loc_string_list_of_instruction c with
  | [] -> [(0, "[]")]
  | [(l, s)] -> [(l, "[" ^ s ^ "]")]
  | (l, s) :: t -> (l, "[" ^ s) :: (apply_to_last (fun (l, s) -> (l, s ^ "]")) t)

and loc_string_list_of_instruction : Past.loc instruction -> (int * string) list = function
  | UNARY({pos_lnum = lnum; _}, op)     -> [(lnum, "UNARY " ^ (string_of_uop op))]
  | OPER({pos_lnum = lnum; _}, op)      -> [(lnum, "OPER " ^ (string_of_bop op))]
  | MK_PAIR {pos_lnum = lnum; _}        -> [(lnum, "MK_PAIR")]
  | FST {pos_lnum = lnum; _}            -> [(lnum, "FST")]
  | SND {pos_lnum = lnum; _}            -> [(lnum, "SND")]
  | MK_INL {pos_lnum = lnum; _}         -> [(lnum, "MK_INL")]
  | MK_INR {pos_lnum = lnum; _}         -> [(lnum, "MK_INR")]
  | MK_REF {pos_lnum = lnum; _}         -> [(lnum, "MK_REF")]
  | PUSH({pos_lnum = lnum; _}, v)       -> [(lnum, "PUSH " ^ (string_of_value v))]
  | LOOKUP({pos_lnum = lnum; _}, x)     -> [(lnum, "LOOKUP " ^ x)]
  | TEST({pos_lnum = lnum; _}, c1, c2)  -> (lnum, "TEST(") :: (tab @@ loc_string_list_of_code c1) @ (tab @@ loc_string_list_of_code c2) @ [(lnum, ")")]
  | CASE({pos_lnum = lnum; _}, c1, c2)  -> (lnum, "CASE(") :: (tab @@ loc_string_list_of_code c1) @ (tab @@ loc_string_list_of_code c2) @ [(lnum, ")")]
  | WHILE({pos_lnum = lnum; _}, c1, c2) -> (lnum, "WHILE(") :: (tab @@ loc_string_list_of_code c1) @ (tab @@ loc_string_list_of_code c2) @ [(lnum, ")")]
  | APPLY {pos_lnum = lnum; _}          -> [(lnum, "APPLY")]
  | BIND({pos_lnum = lnum; _}, x)       -> [(lnum, "BIND " ^ x)]
  | SWAP {pos_lnum = lnum; _}           -> [(lnum, "SWAP")]
  | POP {pos_lnum = lnum; _}            -> [(lnum, "POP")]
  | DEREF {pos_lnum = lnum; _}          -> [(lnum, "DEREF")]
  | ASSIGN {pos_lnum = lnum; _}         -> [(lnum, "ASSIGN")]
  | MK_CLOSURE({pos_lnum = lnum; _}, c) -> (lnum, "MK_CLOSURE(") :: (tab @@ loc_string_list_of_code c) @ [(lnum, ")")]
  | MK_REC({pos_lnum = lnum; _}, f, c)  -> (lnum, "MK_REC(" ^ f ^ ", ") :: (tab @@ loc_string_list_of_code c) @ [(lnum, ")")]

and tab ss = List.map (fun (a, s) -> (a, "\t" ^ s)) ss
let rec driver state =
  match state with
    | ([], _, _) -> [state]
    | _ -> state :: driver (Interp_2.step state)

let steps e =
  let c = Interp_2.compile e
  in driver (initial_state c)

let string_list_of_env env = List.map Interp_2.string_of_env_or_value env

let list_of_map m = List.of_seq @@ Seq.map (fun (_, v) -> v) @@ Interp_2.IntMap.to_seq m

let string_list_of_heap (heap, _) = List.map Interp_2.string_of_value (list_of_map heap)

let loc_string_lists_of_steps steps = List.map (fun (c, e, s) -> (loc_string_list_of_code c, string_list_of_env e, string_list_of_heap s)) steps

let js_string_of_steps steps = Js_of_ocaml.Js.string @@ Yojson.Safe.to_string @@ [%yojson_of: ((int * string) list * string list * string list) list] @@ loc_string_lists_of_steps steps



(* Export a representation of each interpreter step in a stream *)

let rec nsteps states n =
  match (n, states) with
    | (_, []) -> []
    | (_, ([], _, _)::_) -> states
    | (0, _) -> states
    | (n, state::_) -> nsteps (Interp_2.step state :: states) (n-1)

let rec streamDriver' states n =
  let new_states = (nsteps states n) in
   (object%js
     val steps = js_string_of_steps new_states
     method next = streamDriver' new_states n
  end)

let streamDriver e n =
  let c = Interp_2.compile e in
  streamDriver' [(initial_state c)] n

