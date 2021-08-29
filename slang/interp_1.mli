
type expr_i1 = unit Ast.expr

type address = int

type value = 
     | REF of address 
     | INT of int 
     | BOOL of bool 
     | UNIT
     | PAIR of value * value 
     | INL of value 
     | INR of value 
     | REC_CLOSURE of closure
     | CLOSURE of closure  

and closure = Ast.var * expr_i1 * env

and continuation_action = 
  | UNARY of Ast.unary_oper
  | OPER of Ast.oper * value
  | OPER_FST of expr_i1 * env * Ast.oper
  | ASSIGN of value
  | ASSIGN_FST of expr_i1 * env
  | TAIL of expr_i1 list * env
  | IF of expr_i1 * expr_i1 * env
  | WHILE of expr_i1 * expr_i1 * env
  | MKPAIR of value
  | PAIR_FST of expr_i1 * env
  | FST
  | SND
  | MKINL
  | MKINR
  | MKREF
  | DEREF
  | CASE of Ast.var * expr_i1 * Ast.var * expr_i1 * env
  | APPLY of value
  | ARG of expr_i1 * env

and continuation = continuation_action  list

and binding = Ast.var * value

and env = binding list

type state = 
   | EXAMINE of expr_i1 * env * continuation
   | COMPUTE of continuation * value

val step : state -> state 

val driver : int -> state -> value 

val eval : expr_i1 * env -> value

val interpret : 'a Ast.expr -> value

