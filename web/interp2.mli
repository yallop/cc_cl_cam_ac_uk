open Slanglib

type 'a steps = 'a Interp_2.interp_state list

val steps : 'a Ast.expr -> 'a steps
 
val string_list_of_heap : 'a Interp_2.state -> string list

val string_lists_of_steps : 'a steps -> (string list * string list * string list) list

val loc_string_list_of_instruction : Past.loc Interp_2.instruction -> (int * string) list

val loc_string_list_of_code : Past.loc Interp_2.code -> (int * string) list
