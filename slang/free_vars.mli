

(* 
     free_vars (bvars, e) returns a list of the 
     free vars of e that are not contained in bvars. 

*) 
val free_vars : (Past.var list) * ('a Ast.expr) -> (Past.var list)
