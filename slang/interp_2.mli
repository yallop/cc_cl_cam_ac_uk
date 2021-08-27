
type address = int

type var = string [@@deriving yojson]

type value =
     | REF of address
     | INT of int
     | BOOL of bool
     | UNIT
     | PAIR of value * value
     | INL of value
     | INR of value
     | CLOSURE of closure
     | REC_CLOSURE of code [@@deriving yojson]

and closure = code * env


and instruction =
  | PUSH of value
  | LOOKUP of var
  | UNARY of Ast.unary_oper
  | OPER of Ast.oper
  | ASSIGN
  | SWAP
  | POP
  | BIND of var
  | FST
  | SND
  | DEREF
  | APPLY
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of code
  | MK_REC of var * code
  | TEST of code * code
  | CASE of code * code
  | WHILE of code * code

and code = instruction list

and binding = Ast.var * value

and env = binding list

type env_or_value = EV of env | V of value

type env_value_stack = env_or_value list [@@deriving yojson]

(* array of referenced values together with next unallocated address *)
type state = (value array) * int

type interp_state = code * env_value_stack * state [@@deriving yojson]

val initial_state : state

val initial_env : env_value_stack

val step : interp_state -> interp_state

val compile : Ast.expr -> code

val driver : int -> interp_state -> value * state

val interpret : Ast.expr -> value * state

val string_of_instruction : instruction -> string

val string_of_value : value -> string

val string_of_env_or_value : env_or_value -> string

val string_of_code : code -> string