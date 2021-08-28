open Ast 

let rec inlist x = function 
  | [] ->  false 
  | y :: rest -> if x = y then true else inlist x rest 

(* free_vars (bvars, e) returns a 
    list, with no duplicates, of all free variables 
    of e that are not in the list bvars. 
*) 
let free_vars(bvars, exp) =
    let rec aux bound free = function
    | Var(_, x)              -> if (inlist x bound) ||  (inlist x free) then free else x :: free
    | UnaryOp(_, _, e)       -> aux bound free e
    | Op(_, e1, _, e2)       -> aux bound (aux bound free e1) e2
    | If(_, e1, e2, e3)      -> aux bound (aux bound (aux bound free e1) e2) e3
    | Pair(_, e1, e2)        -> aux bound (aux bound free e1) e2
    | App(_, e1, e2)         -> aux bound (aux bound free e1) e2
    | Fst(_, e)              -> aux bound free e
    | Snd(_, e)              -> aux bound free e
    | Inl(_, e)              -> aux bound free e
    | Inr(_,e)               -> aux bound free e
    | Lambda l               -> lambda bound free l
    | Case(_, e, l1, l2)     -> lambda bound (lambda bound (aux bound free e) l1) l2
    | LetFun(_, f, l, e)     -> aux (f :: bound) (lambda bound free l) e
    | LetRecFun(_, f, l, e)  -> aux (f :: bound) (lambda (f :: bound) free l) e
    | Ref(_, e)              -> aux bound free e
    | Deref(_, e)            -> aux bound free e
    | Assign(_, e1, e2)      -> aux bound (aux bound free e1) e2
    | While(_, e1, e2)       -> aux bound (aux bound free e1) e2
    | Seq(_, [])             -> free
    | Seq(l, (e :: rest))    -> aux bound (aux bound free e) (Seq(l, rest))
    | _                      -> free
    and lambda bound free (_, x, e) = aux (x :: bound) free e
   in aux bvars [] exp

