open Slang

type 'a steps = 'a Interp_2.interp_state list

val steps : 'a Ast.expr -> 'a steps
 
val string_list_of_heap : 'a Interp_2.state -> string list

val string_lists_of_steps : 'a steps -> (string list * string list * string list) list
