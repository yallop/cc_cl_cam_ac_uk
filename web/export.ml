open Js_of_ocaml
open Slang

let stepCount = 40

let wrapCode interp str =
  try interp str
  with 
    | Errors.Error s -> "[[0, \"Error:\\n" ^ String.trim s ^ "\"]]"
    | Failure s -> "[[0, \"Error:\\n" ^ String.trim s ^ "\"]]"

let wrapInterp f errReplacement = 
  try f ()
  with
    | Errors.Error s -> errReplacement s
    | Failure s -> errReplacement s

let yojson_of_location_instructions x = Yojson.Safe.to_string @@ [%yojson_of: (int * string) list] @@ x

let frontend str = Front_end.front_end_from_string (Js.to_string str)
let _ =
  Js.export "slang"
    (object%js
      method interp2Code str = Js.string (wrapCode (fun x ->
        (yojson_of_location_instructions @@ Interp2.loc_string_list_of_code (Interp_2.compile (frontend x)))) str)
      method interp3Code str = Js.string (wrapCode (fun x ->
        (Interp_3.reset(); yojson_of_location_instructions @@ (Interp3.loc_string_list_of_code  (Interp_3.compile (frontend x))))) str)
      method jargonCode  str = Js.string (wrapCode (fun x ->
        (Jargon.reset(); yojson_of_location_instructions @@ JargonSteps.location_string_list_of_code (Jargon.compile (frontend x)))) str)

      method i2Stream str = wrapInterp (fun _ -> Interp2.streamDriver (frontend str) stepCount) Util.errStream
      method i3Stream str = wrapInterp (fun _ -> Interp3.streamDriver (frontend str) stepCount) Interp3.err
      method jargonStream str = wrapInterp (fun _ -> JargonSteps.streamDriver (frontend str) stepCount) JargonSteps.err
    end)
