open Slanglib

type 'a steps = Interp_2.state list

val steps : Ast.t -> 'a steps

val string_lists_of_steps :
  'a steps -> (string list * string list * string list) list

val string_list_of_code : Interp_2.code -> string list
