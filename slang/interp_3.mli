
type address = int 

type label = string 

type location = label * (address option) 

type value = 
  | REF of address 
  | INT of int 
  | BOOL of bool 
  | UNIT
  | PAIR of value * value 
  | INL of value 
  | INR of value 
  | CLOSURE of location * env
  | REC_CLOSURE of location

and 'a instruction =
  | PUSH of 'a * value
  | LOOKUP of 'a * Ast.var
  | UNARY of 'a * Ast.unary_oper
  | OPER of 'a * Ast.oper
  | ASSIGN of 'a
  | SWAP of 'a
  | POP of 'a
  | BIND of 'a * Ast.var
  | FST of 'a
  | SND of 'a
  | DEREF of 'a
  | APPLY of 'a
  | RETURN of 'a
  | MK_PAIR of 'a
  | MK_INL of 'a
  | MK_INR of 'a
  | MK_REF of 'a
  | MK_CLOSURE of 'a * location
  | MK_REC of 'a * Ast.var * location
  | TEST of 'a * location
  | CASE of 'a * location
  | GOTO of 'a * location
  | LABEL of 'a * label
  | HALT of 'a

and 'a code = 'a instruction list

and binding = Ast.var * value

and env = binding list

type env_or_value = 
  | EV of env           (* an environment on the run-time stack *) 
  | V of value          (* a value on the run-time stack *) 
  | RA of address    (* a return address on the run-time stack *) 

type env_value_stack = env_or_value list 

type state = address * env_value_stack 

val installed : (unit instruction array) ref

val load : 'a instruction list -> 'a instruction array

val step : state -> state 

val compile : 'a Ast.expr -> 'a code

val heap : value array

val next_address : address ref

val driver : int -> state -> value 

val get_instruction : address -> unit instruction

val interpret : 'a Ast.expr -> value

val string_of_code : 'a code -> string

val string_of_value : value -> string 

val string_of_env_or_value : env_or_value -> string

val string_of_installed_code : unit -> string

val string_of_location : location -> string

val reset : unit -> unit

val map : ('a -> 'b) -> 'a instruction -> 'b instruction
