open Slanglib
open Ast
open Interp_2

type 'a steps = Interp_2.interp_state list

let rec driver state =
  match state with
  | [], _, _ -> [ state ]
  | _ -> state :: driver (Interp_2.step state)

let steps e =
  let c = Interp_2.compile e in
  driver (c, Interp_2.initial_env, Interp_2.initial_state)

let string_list_of_code code = List.map Interp_2.string_of_instruction code
let string_list_of_env env = List.map Interp_2.string_of_env_or_value env

let list_of_map m =
  List.of_seq @@ Seq.map (fun (_, v) -> v) @@ Interp_2.IntMap.to_seq m

let string_list_of_heap (heap, _) =
  List.map Interp_2.string_of_value (list_of_map heap)

let string_lists_of_steps steps =
  List.map
    (fun (c, e, s) ->
      (string_list_of_code c, string_list_of_env e, string_list_of_heap s))
    steps

let apply_to_last f l =
  let length = List.length l - 1 in
  List.mapi
    (fun i x ->
      if length = i then
        f x
      else
        x)
    l

let rec string_list_of_code c =
  match List.flatten @@ List.map string_list_of_instruction c with
  | [] -> [ "[]" ]
  | [ s ] -> [ "[" ^ s ^ "]" ]
  | s :: t -> ("[" ^ s) :: apply_to_last (fun s -> s ^ "]") t

and string_list_of_instruction : instruction -> string list = function
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
  | TEST (c1, c2) ->
      ("TEST(" :: (tab @@ string_list_of_code c1))
      @ (tab @@ string_list_of_code c2)
      @ [ ")" ]
  | CASE (c1, c2) ->
      ("CASE(" :: (tab @@ string_list_of_code c1))
      @ (tab @@ string_list_of_code c2)
      @ [ ")" ]
  | WHILE (c1, c2) ->
      ("WHILE(" :: (tab @@ string_list_of_code c1))
      @ (tab @@ string_list_of_code c2)
      @ [ ")" ]
  | APPLY -> [ "APPLY" ]
  | BIND x -> [ "BIND " ^ x ]
  | SWAP -> [ "SWAP" ]
  | POP -> [ "POP" ]
  | DEREF -> [ "DEREF" ]
  | ASSIGN -> [ "ASSIGN" ]
  | MK_CLOSURE c -> ("MK_CLOSURE(" :: (tab @@ string_list_of_code c)) @ [ ")" ]
  | MK_REC (f, c) ->
      (("MK_REC(" ^ f ^ ", ") :: (tab @@ string_list_of_code c)) @ [ ")" ]

and tab ss = List.map (fun s -> "\t" ^ s) ss
