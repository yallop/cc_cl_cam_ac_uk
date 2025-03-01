open Slanglib

type 'a steps = Interp_2.interp_state list

val steps : Ast.expr -> 'a steps

val string_list_of_heap : Interp_2.state -> string list

val string_lists_of_steps : 'a steps -> (string list * string list * string list) list

val string_list_of_instruction : Interp_2.instruction -> string list

val string_list_of_code : Interp_2.code -> string list
