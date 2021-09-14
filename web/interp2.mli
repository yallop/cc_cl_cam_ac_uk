open Slang

type 'a steps = 'a Interp_2.interp_state list

val steps : Past.loc Ast.expr -> Past.loc steps
 
val string_list_of_heap : 'a Interp_2.state -> string list

val loc_string_lists_of_steps : Past.loc steps -> ((int * string) list * string list * string list) list

val loc_string_list_of_instruction : Past.loc Interp_2.instruction -> (int * string) list

val loc_string_list_of_code : Past.loc Interp_2.code -> (int * string) list

val streamDriver : Past.loc Ast.expr -> int -> string Util.stream