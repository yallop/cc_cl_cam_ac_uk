(**************************************
Compiler Construction 2016
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

open Slanglib

(* This is the main file. *)

let error file action s =
  Format.printf "ERROR in %s with %s : %s\n" file action s

let fatal_error file action s =
  error file action s;
  exit (-1)

(* bind interpreters *)
(* each interpreter i_ has type "(string * Ast.expr) -> string option" *)

let wrap (file : string) (e : Ast.t) (interpret : Ast.t -> string)
    (msg : string) =
  match interpret e with
  | v -> Some v
  | exception Errors.Error s ->
      error file msg s;
      None
  | exception exc ->
      fatal_error file msg ("Exception: " ^ Printexc.to_string exc)

let i0 (file, e) =
  wrap file e
    (fun x -> Interp_0.show_value (Interp_0.interpret x))
    "Interpreter 0"

let i1 (file, e) =
  wrap file e
    (fun x -> Interp_1.show_value (Interp_1.interpret x))
    "Interpreter 1"

let i2 (file, e) =
  wrap file e
    (fun x -> Interp_2.show_value (Interp_2.interpret x))
    "Interpreter 2"

let i3 (file, e) =
  wrap file e
    (fun x -> Interp_3.show_value (Interp_3.interpret x))
    "Interpreter 3"

let i4 (file, e) =
  wrap file e
    (fun x -> Format.asprintf "%a" Jargon.pp_value (Jargon.interpret x))
    "Jargon VM"

let i4x86 (_, e) =
  let _ = Jargon_to_x86.emit_x86 e in
  None

(* show compiled code *)
let i2cc (_, e) =
  Format.printf "%a" Interp_2.pp_code (Interp_2.compile e);
  None

let i3cc (_, e) =
  Format.printf "%a" Interp_3.pp_code (Interp_3.compile e);
  None

let i4cc (_, e) =
  Format.printf "%a" Jargon.pp_code (Jargon.compile e);
  None

let interpreters =
  [
    (* use-flag, the action, a description string *)
    (Option.use_i0, i0, "Interpreter 0");
    (Option.use_i1, i1, "Interpreter 1");
    (Option.use_i2 && not Option.show_compiled, i2, "Interpreter 2");
    (Option.use_i2 && Option.show_compiled, i2cc, "Interpreter 2, compiled code");
    (Option.use_i3 && not Option.show_compiled, i3, "Interpreter 3");
    (Option.use_i3 && Option.show_compiled, i3cc, "Interpreter 3, compiled code");
    (Option.use_i4 && not Option.show_compiled, i4, "Jargon VM");
    (Option.use_i4x86, i4x86, "Jargon code to x86");
    (Option.use_i4 && Option.show_compiled, i4cc, "Jargon, compiled code");
  ]
  |> List.filter_map (fun (cond, action, description) ->
         if cond then
           Some (action, description)
         else
           None)

let show_output describe string_out =
  if not Option.run_tests then (
    if Option.verbose then
      Format.printf "%s \n" describe;
    Format.printf "output> %s\n" string_out)

(* used for -t option *)
let check_expected describe file string_out = function
  | None -> ()
  | Some expected when string_out = expected -> ()
  | Some expected ->
      error file "testing"
        (describe ^ " computed " ^ string_out ^ ", but expected " ^ expected)

(* runs all interpreters on expression e : Ast.expr.
   If expected_option = Some (expected), then
   checks if the correct result was computed (silent otherwise).
*)

(* process_inputs : runs all (flagged) interpreters on all inputs *)
let process_input (file, expected) =
  try
    let e = Front_end.front_end file in
    List.iter
      (fun (interp, description) ->
        match interp (file, e) with
        | None -> ()
        | Some string_out ->
            show_output description string_out;
            check_expected description file string_out expected)
      interpreters
  with Errors.Error s -> error file "Front_end" s

let () =
  if Option.run_tests then
    try List.iter process_input Tests.manifest
    with Errors.Error s -> fatal_error "tests/" "Test.get_all_tests" s
  else
    process_input (Option.infile, None)
