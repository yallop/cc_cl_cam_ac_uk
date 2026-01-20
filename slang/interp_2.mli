type f

type value = f Value.t
and closure = code * env

and instruction =
  | PUSH of value
  | LOOKUP of Ast.var
  | UNARY of Ast.Unary_op.t
  | OPER of Ast.Binary_op.t
  | ASSIGN
  | SWAP
  | POP
  | BIND of Ast.var
  | FST
  | SND
  | DEREF
  | APPLY
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of code
  | MK_REC of Ast.var * code
  | TEST of code * code
  | CASE of code * code
  | WHILE of code * code

and code = instruction list
and binding = Ast.var * value
and env = binding list

type env_or_value = EV of env | V of value
type stack = env_or_value list
type state = { code : code; stack : stack; heap : value Heap.t }

val step : state -> state
val compile : Ast.t -> code
val interpret : Ast.t -> value
val pp_instruction : Format.formatter -> instruction -> unit
val pp_value : Format.formatter -> value -> unit
val show_value : value -> string
val pp_env_or_value : Format.formatter -> env_or_value -> unit
val pp_code : Format.formatter -> code -> unit
