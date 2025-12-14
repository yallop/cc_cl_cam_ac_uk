(** [free_vars bound e] returns a list, with no duplicates, of all free
    variables of [e] that are not in the list [bound]. *)
let free_vars (bound : Ast.var list) (exp : Ast.t) : Ast.var list =
  let rec aux (bound : Ast.var list) (free : Ast.var list) :
      Ast.t -> Ast.var list = function
    | Var x ->
        if List.mem x bound || List.mem x free then
          free
        else
          x :: free
    | UnaryOp (_, e) -> aux bound free e
    | BinaryOp (e1, _, e2) -> aux bound (aux bound free e1) e2
    | If (e1, e2, e3) -> aux bound (aux bound (aux bound free e1) e2) e3
    | Pair (e1, e2) -> aux bound (aux bound free e1) e2
    | App (e1, e2) -> aux bound (aux bound free e1) e2
    | Fst e -> aux bound free e
    | Snd e -> aux bound free e
    | Inl e -> aux bound free e
    | Inr e -> aux bound free e
    | Lambda l -> lambda bound free l
    | Case (e, l1, l2) -> lambda bound (lambda bound (aux bound free e) l1) l2
    | LetFun (f, l, e) -> aux (f :: bound) (lambda bound free l) e
    | LetRecFun (f, l, e) -> aux (f :: bound) (lambda (f :: bound) free l) e
    | Ref e -> aux bound free e
    | Deref e -> aux bound free e
    | Assign (e1, e2) -> aux bound (aux bound free e1) e2
    | While (e1, e2) -> aux bound (aux bound free e1) e2
    | Seq [] -> free
    | Seq (e :: rest) -> aux bound (aux bound free e) (Seq rest)
    | _ -> free
  and lambda bound free (x, e) = aux (x :: bound) free e in
  aux bound [] exp
