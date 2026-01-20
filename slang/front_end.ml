let parse_error filename (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let line = pos.pos_lnum in
  let pos = pos.pos_cnum - pos.pos_bol + 1 in
  Errors.complainf "ERROR in %s with parsing : at line %d position %d@."
    filename line pos

(** Parse input file *)
let parse file lexbuf =
  let e =
    try Parser.start Lexer.token lexbuf
    with Parsing.Parse_error -> parse_error file lexbuf
  in
  if Option.verbose_front then
    if Option.verbose_tree then
      Format.printf "==== Parsed result ====@\n%a@\n" Past.pp_nice e
    else
      Format.printf "==== Parsed result ====@\n%a@\n" Past.pp e;
  e

(** Perform static checks and translate from Past to Ast *)
let check filename e =
  let e' =
    try Static.translate e
    with Static.Type_error e ->
      Errors.complainf "ERROR in %s with type checking at %a:\n%s\n" filename
        Past.Loc.pp_nice e.loc e.message
  in
  if Option.verbose_front then
    if Option.verbose_tree then
      Format.printf "==== After static checks ====@.%a@." Ast.pp_nice e'
    else
      Format.printf "==== After static checks ====@.%a@." Ast.pp e';

  e'

(** The front end *)
let front_end filename =
  let in_chan =
    try open_in filename
    with _ ->
      Errors.complainf "ERROR in %s with initializing lexer : can't open file"
        filename
  in
  let lexbuf = Lexing.from_channel in_chan in

  lexbuf.lex_curr_p <-
    { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };

  lexbuf |> parse filename |> check filename

let front_end_from_string str =
  let filename = "input" in
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <-
    { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  lexbuf |> parse filename |> check filename
