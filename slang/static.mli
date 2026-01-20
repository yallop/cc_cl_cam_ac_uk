exception Type_error of { loc : Past.Loc.t; message : string }

val translate : Past.t -> Ast.t
