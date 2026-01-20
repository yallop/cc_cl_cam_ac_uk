open Slanglib
open Interp_2

type 'a steps = Interp_2.state list

let rec driver state =
  match state.code with
  | [] -> [ state ]
  | _ -> state :: driver (Interp_2.step state)

let steps e =
  let c = Interp_2.compile e in
  driver { code = c; stack = []; heap = Heap.empty }

let string_list_of_code code =
  List.map (Format.asprintf "%a" Interp_2.pp_instruction) code

let string_list_of_stack env =
  List.map (Format.asprintf "%a" Interp_2.pp_env_or_value) env

let string_list_of_heap heap =
  List.map
    (Format.asprintf "%a" (Heap.pp_binding Interp_2.pp_value))
    (Heap.to_list heap)

let string_lists_of_steps steps =
  List.map
    (fun { code; stack; heap } ->
      ( string_list_of_code code,
        string_list_of_stack stack,
        string_list_of_heap heap ))
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
  | UNARY op -> [ Format.asprintf "UNARY %a" Ast.Unary_op.pp op ]
  | OPER op -> [ Format.asprintf "OPER %a" Ast.Binary_op.pp op ]
  | MK_PAIR -> [ "MK_PAIR" ]
  | FST -> [ "FST" ]
  | SND -> [ "SND" ]
  | MK_INL -> [ "MK_INL" ]
  | MK_INR -> [ "MK_INR" ]
  | MK_REF -> [ "MK_REF" ]
  | PUSH v -> [ Format.asprintf "PUSH %a" Interp_2.pp_value v ]
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
