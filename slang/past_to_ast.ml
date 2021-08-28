

(*   translate_expr : Past.expr -> Ast.expr 
     Translates parsed AST to internal AST : 
     1) drop types 
     2) remove let 
     ) replace "?" (What) with unary function call 

  Note : our front-end drops type information.  Is this really a good idea? 
  Could types be useful in later phases of the compiler? 

*) 

let translate_uop = function 
  | Past.NEG -> Ast.NEG 
  | Past.NOT -> Ast.NOT 

let translate_bop = function 
  | Past.ADD -> Ast.ADD 
  | Past.MUL -> Ast.MUL
  | Past.DIV -> Ast.DIV
  | Past.SUB -> Ast.SUB
  | Past.LT -> Ast.LT
  | Past.AND -> Ast.AND
  | Past.OR -> Ast.OR
  | Past.EQI -> Ast.EQI
  | Past.EQB -> Ast.EQB
  | Past.EQ  -> Errors.complain "internal error, translate found a EQ that should have been resolved to EQI or EQB"


let rec translate_expr = function
    | Past.Unit l            -> Ast.Unit l
    | Past.What l            -> Ast.UnaryOp(l, Ast.READ, (Ast.Unit l))
    | Past.Var(l, x)         -> Ast.Var(l, x)
    | Past.Integer(l, n)     -> Ast.Integer(l, n)
    | Past.Boolean(l, b)     -> Ast.Boolean(l, b)
    | Past.UnaryOp(l, op, e) -> Ast.UnaryOp(l, translate_uop op, translate_expr e)
    | Past.Op(l, e1, op, e2) -> Ast.Op(l, translate_expr e1, translate_bop op, translate_expr e2)
    | Past.If(l, e1, e2, e3) -> Ast.If(l, translate_expr e1, translate_expr e2, translate_expr e3)
    | Past.Pair(l, e1, e2)   -> Ast.Pair(l, translate_expr e1, translate_expr e2)
    | Past.Fst(l, e)         -> Ast.Fst(l, translate_expr e)
    | Past.Snd(l, e)         -> Ast.Snd(l, translate_expr e)
    | Past.Inl(l, _, e)       -> Ast.Inl(l, translate_expr e)
    | Past.Inr(l, _, e)       -> Ast.Inr(l, translate_expr e)
    | Past.Case(l, e, l1, l2) ->
         Ast.Case(l, translate_expr e, translate_lambda l l1, translate_lambda l l2)
    | Past.Lambda(l, lam)      -> Ast.Lambda(translate_lambda l lam)
    | Past.App(l, e1, e2)    -> Ast.App(l, translate_expr e1, translate_expr e2)
    (* 
       Replace "let" with abstraction and application. For example, translate 
        "let x = e1 in e2 end" to "(fun x -> e2) e1" 
    *) 
    | Past.Let(l, x, _, e1, e2) ->
         Ast.App(l, Ast.Lambda(l, x, translate_expr e2), translate_expr e1)
    | Past.LetFun(l, f, lam, _, e)     ->
         Ast.LetFun(l, f, translate_lambda l lam, translate_expr e)
    | Past.LetRecFun(l, f, lam, _, e)     ->
         Ast.LetRecFun(l, f, translate_lambda l lam, translate_expr e)

    | Past.Seq(l, el) -> Ast.Seq(l, List.map translate_expr el)
    | Past.While(l, e1, e2) -> Ast.While(l, translate_expr e1, translate_expr e2)
    | Past.Ref(l, e) -> Ast.Ref(l, translate_expr e)
    | Past.Deref(l, e) -> Ast.Deref(l, translate_expr e)
    | Past.Assign(l, e1, e2) -> Ast.Assign(l, translate_expr e1, translate_expr e2)

and translate_lambda l (x, _, body) = (l, x, translate_expr body)
