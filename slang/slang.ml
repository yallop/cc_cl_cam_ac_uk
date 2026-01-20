(**************************************
Compiler Construction 2016
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

open Slanglib

(*  This is the main file. *)

let error file action s =
  print_string ("\nERROR in " ^ file ^ " with " ^ action ^ " : " ^ s ^ "\n")

let fatal_error file action s =
  error file action s;
  exit (-1)

(* bind interpreters *)
(* each interpreter i_ has type "(string * Ast.expr) -> string option" *)

let wrap file e interpret msg =
  match interpret e with
  | v -> Some v
  | exception Errors.Error s ->
      error file msg s;
      None
  | exception exc ->
      fatal_error file msg ("Exception: " ^ Printexc.to_string exc)

let i0 (file, e) =
  wrap file e
    (fun x -> Interp_0.string_of_value (Interp_0.interpret_top_level x))
    "Interpreter 0"

let i1 (file, e) =
  wrap file e
    (fun x -> Interp_1.string_of_value (Interp_1.interpret x))
    "Interpreter 1"

let i2 (file, e) =
  wrap file e
    (fun x -> Interp_2.string_of_value (fst (Interp_2.interpret x)))
    "Interpreter 2"

let i3 (file, e) =
  wrap file e
    (fun x -> Interp_3.string_of_value (Interp_3.interpret x))
    "Interpreter 3"

let i4 (file, e) =
  wrap file e (fun x -> Jargon.string_of_value (Jargon.interpret x)) "Jargon VM"

let i4x86 (_, e) =
  let _ = Jargon_to_x86.emit_x86 e in
  None

(* show compiled code *)
let i2cc (_, e) =
  let () = print_endline (Interp_2.string_of_code (Interp_2.compile e)) in
  None

let i3cc (_, e) =
  let () = print_endline (Interp_3.string_of_code (Interp_3.compile e)) in
  None

let i4cc (_, e) =
  let () = print_endline (Jargon.string_of_listing (Jargon.compile e)) in
  None

let interpreters =
  [
    (* use-flag, the action, a description string *)
    (Option.use_i0, i0, "Interpreter 0");
    (Option.use_i1, i1, "Interpreter 1");
    (Option.use_i2 && not Option.show_compiled, i2, "Interpreter 2");
    (Option.show_compiled && Option.use_i2, i2cc, "Interpreter 2, compiled code");
    (Option.use_i3 && not Option.show_compiled, i3, "Interpreter 3");
    (Option.show_compiled && Option.use_i3, i3cc, "Interpreter 3, compiled code");
    (Option.use_i4 && not Option.show_compiled, i4, "Jargon VM");
    (Option.use_i4x86, i4x86, "Jargon code to x86");
    (Option.show_compiled && Option.use_i4, i4cc, "Jargon, compiled code");
  ]

let show_output describe string_out =
  if not Option.run_tests then begin
    if Option.verbose then
      print_string ("\n" ^ describe ^ " \n");
    print_string ("output> " ^ string_out ^ "\n")
  end

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
let rec run_interpreters file e expected_option = function
  | [] -> ()
  | (true, interp, describe) :: rest -> (
      match interp (file, e) with
      | None -> run_interpreters file e expected_option rest
      | Some string_out ->
          let () = show_output describe string_out in
          let () = check_expected describe file string_out expected_option in
          run_interpreters file e expected_option rest)
  | (false, _, _) :: rest -> run_interpreters file e expected_option rest

(* process_inputs : runs all (flagged) interpreters on all inputs *)
let process_input (file, expected) =
  try
    let e = Front_end.front_end file in
    run_interpreters file e expected interpreters
  with Errors.Error s -> error file "Front_end" s

let process_inputs = List.iter process_input

let () =
  process_inputs
    (if Option.run_tests then
       try
         Tests.get_all_tests ()
       with Errors.Error s -> fatal_error "tests/" "Test.get_all_tests" s
     else
       [ (Option.infile, None) ])
