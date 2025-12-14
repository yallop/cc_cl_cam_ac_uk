type f
type value = f Value.t

val show_value : value -> string
val interpret : Ast.t -> value
