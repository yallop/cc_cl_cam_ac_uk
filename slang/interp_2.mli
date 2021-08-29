(* Using IntMap to represent memory *)
module IntMap : Map.S with type key = int

type address = int 

type var = string

type 'a value =
     | REF of address 
     | INT of int 
     | BOOL of bool 
     | UNIT
     | PAIR of 'a value * 'a value
     | INL of 'a value
     | INR of 'a value
     | CLOSURE of 'a closure
     | REC_CLOSURE of 'a code

and 'a closure = 'a code * 'a env


and 'a instruction =
  | PUSH of 'a * 'a value
  | LOOKUP of 'a * var
  | UNARY of 'a * Ast.unary_oper
  | OPER of 'a * Ast.oper
  | ASSIGN of 'a
  | SWAP of 'a
  | POP of 'a
  | BIND of 'a * var
  | FST of 'a
  | SND of 'a
  | DEREF of 'a
  | APPLY of 'a
  | MK_PAIR of 'a
  | MK_INL of 'a
  | MK_INR of 'a
  | MK_REF of 'a
  | MK_CLOSURE of 'a * 'a code
  | MK_REC of 'a * var * 'a code
  | TEST of 'a * 'a code * 'a code
  | CASE of 'a * 'a code * 'a code
  | WHILE of 'a * 'a code * 'a code

and 'a code = 'a instruction list

and 'a binding = Ast.var * 'a value

and 'a env = 'a binding list

type 'a env_or_value = EV of 'a env | V of 'a value

type 'a env_value_stack = 'a env_or_value list

(* array of referenced values together with next unallocated address *)
type 'a state = ('a value IntMap.t) * int

type 'a interp_state = 'a code * 'a env_value_stack * 'a state

val initial_state : 'a state

val initial_env : 'a env_value_stack

val step :  'a interp_state -> 'a interp_state

val compile : 'a Ast.expr -> 'a code

val driver : int -> 'a interp_state -> 'a value * 'a state

val interpret : 'a Ast.expr -> 'a value * 'a state

val string_of_instruction : 'a instruction -> string

val string_of_value : 'a value -> string

val string_of_env_or_value : 'a env_or_value -> string

val string_of_code : 'a code -> string
