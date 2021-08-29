open Slang

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
