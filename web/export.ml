open Js_of_ocaml
open Slang

let wrap interp str =
  try interp str
  with 
    | Errors.Error s -> s

let wrap_yojson_string f = Js.string (
  try (Yojson.Safe.to_string (f()))
  with Errors.Error _ -> "\"Error\""
)

type egg = EGG [@@deriving yojson]
let frontend str = Front_end.front_end_from_string (Js.to_string str)
let _ =
  Js.export "slang"
    (object%js
      method interp0     str = Js.string (wrap (fun x ->
        (Interp_0.string_of_value (Interp_0.interpret_top_level (frontend x)))) str)
        
      method interp2     str = wrap_yojson_string (fun _ -> (
        [%yojson_of: (string list * string list * string list) list] (Interp2.string_lists_of_steps (Interp2.steps @@ frontend str))))
      method interp3     str = wrap_yojson_string (fun _ -> ( Interp_3.reset();
        ([%yojson_of: string * (int * string list * string list) list] (Interp3.stacks (frontend str)))))
      method jargon      str = wrap_yojson_string (fun _ -> (Jargon.reset();
        ([%yojson_of: string list * JargonSteps.ret list] (JargonSteps.steps (frontend str)))))

      method interp2Code str = Js.string (wrap (fun x ->
        (Interp_2.string_of_code  (Interp_2.compile (frontend x)))) str)
      method interp3Code str = Js.string (wrap (fun x ->
        (Interp_3.reset(); (Interp_3.string_of_code  (Interp_3.compile (frontend x))))) str)
      method jargonCode  str = Js.string (wrap (fun x ->
        (Jargon.reset(); Jargon.string_of_listing (Jargon.compile (frontend x)))) str)
    end)
