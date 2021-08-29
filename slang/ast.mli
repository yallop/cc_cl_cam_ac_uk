
type var = string 

type oper = ADD | MUL | DIV | SUB | LT | AND | OR | EQB | EQI

type unary_oper = NEG | NOT | READ

type 'a expr =
       | Unit of 'a
       | Var of 'a * var
       | Integer of 'a * int
       | Boolean of 'a * bool
       | UnaryOp of 'a * unary_oper * 'a expr
       | Op of 'a * 'a expr * oper * 'a expr
       | If of 'a * 'a expr * 'a expr * 'a expr
       | Pair of 'a * 'a expr * 'a expr
       | Fst of 'a * 'a expr
       | Snd of 'a * 'a expr
       | Inl of 'a * 'a expr
       | Inr of 'a * 'a expr
       | Case of 'a * 'a expr * 'a lambda * 'a lambda

       | While of 'a * 'a expr * 'a expr
       | Seq of 'a * 'a expr list
       | Ref of 'a * 'a expr
       | Deref of 'a * 'a expr
       | Assign of 'a * 'a expr * 'a expr

       | Lambda of 'a lambda
       | App of 'a * 'a expr * 'a expr
       | LetFun of 'a * var * 'a lambda * 'a expr
       | LetRecFun of 'a * var * 'a lambda * 'a expr

and 'a lambda = 'a * Past.var * 'a expr

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_uop : unary_oper -> string 
val string_of_bop : oper -> string 
val print_expr : 'a expr -> unit
val eprint_expr : 'a expr -> unit
val string_of_expr : 'a expr -> string
val map : ('a -> 'b) -> 'a expr -> 'b expr
val get_tag : 'a expr -> 'a
