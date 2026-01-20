type address = int
type label
type location
type f
type value = f Value.t

type instruction =
  | PUSH of value
  | LOOKUP of label
  | UNARY of Ast.Unary_op.t
  | OPER of Ast.Binary_op.t
  | ASSIGN
  | SWAP
  | POP
  | BIND of label
  | FST
  | SND
  | DEREF
  | APPLY
  | RETURN
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of location
  | MK_REC of label * location
  | TEST of location
  | CASE of location
  | GOTO of location
  | LABEL of label
  | HALT

and env

val show_value : value -> string

type env_or_value
type env_value_stack

val pp_env_or_value : Format.formatter -> env_or_value -> unit

type state = { heap : value Heap.t; cp : address; stack : env_value_stack }

val init_state : state
val lookup : string -> env -> value option
val pp_fun : Format.formatter -> f -> unit
val pp_location : Format.formatter -> location -> unit
val pp_instruction : Format.formatter -> instruction -> unit
val pp_code : Format.formatter -> instruction list -> unit
val step : instruction array -> state -> state
val compile : Ast.t -> instruction list
val load : instruction list -> instruction array
val interpret : Ast.t -> value
