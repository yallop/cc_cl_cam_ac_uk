open Slanglib

let string_state (cp, evs, heap_list) =
  ( cp,
    List.map (Format.asprintf "%a" Interp_3.pp_env_or_value) evs,
    List.map Interp_3.show_value heap_list )

let rec driver n code (state : Interp_3.state) =
  let heapl = Heap.to_list state.heap in
  (state.cp, state.stack, heapl)
  ::
  (if Interp_3.HALT = code.(state.cp) then
     []
   else
     driver (n + 1) code (Interp_3.step code state))

let stacks e =
  let c = Interp_3.compile e in
  let installed_code = Format.asprintf "%a" Interp_3.pp_code c in
  let _ = Interp_3.load c in
  (installed_code, [ (0, [ "" ], [ "" ]) ])

let string_list_of_code c =
  List.flatten
  @@ List.map
       (fun instr ->
         let buf = Buffer.create 16 in
         let ppf = Format.formatter_of_buffer buf in
         Interp_3.pp_instruction ppf instr;
         Format.pp_print_flush ppf ();
         [ Buffer.contents buf ])
       c
(*
Show the compiled code, and the state at each step of the execution.
*)
