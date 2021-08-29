
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

and 'a lambda = 'a * var * 'a expr


open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 

let pp_uop = function 
  | NEG -> "-" 
  | NOT -> "~" 
  | READ -> "read" 


let pp_bop = function 
  | ADD -> "+" 
  | MUL  -> "*" 
  | DIV  -> "/" 
  | SUB -> "-" 
  | LT   -> "<" 
  | EQI   -> "eqi" 
  | EQB   -> "eqb" 
  | AND   -> "&&" 
  | OR   -> "||" 


let string_of_oper = pp_bop 
let string_of_unary_oper = pp_uop 

let fstring ppf s = fprintf ppf "%s" s

let pp_unary ppf t = fstring ppf (pp_uop t) 

let pp_binary ppf t = fstring ppf (pp_bop t) 

let rec pp_expr ppf = function
    | Unit _               -> fstring ppf "()"
    | Var(_, x)            -> fstring ppf x
    | Integer(_, n)        -> fstring ppf (string_of_int n)
    | Boolean(_, b)        -> fstring ppf (string_of_bool b)
    | UnaryOp(_, op, e)    -> fprintf ppf "%a(%a)" pp_unary op pp_expr e
    | Op(_, e1, op, e2)    -> fprintf ppf "(%a %a %a)" pp_expr e1  pp_binary op pp_expr e2
    | If(_, e1, e2, e3)    -> fprintf ppf "@[if %a then %a else %a @]"
                                      pp_expr e1 pp_expr e2 pp_expr e3
    | Pair(_, e1, e2)      -> fprintf ppf "(%a, %a)" pp_expr e1 pp_expr e2
    | Fst(_, e)            -> fprintf ppf "fst(%a)" pp_expr e
    | Snd(_, e)            -> fprintf ppf "snd(%a)" pp_expr e
    | Inl(_, e)            -> fprintf ppf "inl(%a)" pp_expr e
    | Inr(_, e)            -> fprintf ppf "inr(%a)" pp_expr e
    | Case(_, e, (_, x1, e1), (_, x2, e2)) ->
        fprintf ppf "@[<2>case %a of@ | inl %a -> %a @ | inr %a -> %a end@]"
                     pp_expr e fstring x1 pp_expr e1 fstring x2 pp_expr e2
    | Lambda(_, x, e) ->
         fprintf ppf "(fun %a -> %a)" fstring x pp_expr e
    | App(_, e1, e2)       -> fprintf ppf "%a %a" pp_expr e1 pp_expr e2

    | Seq(_, el)           -> fprintf ppf "begin %a end" pp_expr_list el
    | While(_, e1, e2)     -> fprintf ppf "while %a do %a end" pp_expr e1 pp_expr e2
    | Ref(_, e)            -> fprintf ppf "ref(%a)" pp_expr e
    | Deref(_, e)          -> fprintf ppf "!(%a)" pp_expr e
    | Assign(_, e1, e2)    -> fprintf ppf "(%a := %a)" pp_expr e1 pp_expr e2
    | LetFun(_, f, (_, x, e1), e2)     ->
         fprintf ppf "@[let %a(%a) =@ %a @ in %a @ end@]"
                     fstring f fstring x  pp_expr e1 pp_expr e2
    | LetRecFun(_, f, (_, x, e1), e2)  ->
         fprintf ppf "@[letrec %a(%a) =@ %a @ in %a @ end@]"
                     fstring f fstring x  pp_expr e1 pp_expr e2
and pp_expr_list ppf = function 
  | [] -> () 
  | [e] -> pp_expr ppf e 
  |  e:: rest -> fprintf ppf "%a; %a" pp_expr e pp_expr_list rest 


let print_expr e = 
    let _ = pp_expr std_formatter e
    in print_flush () 

let eprint_expr e = 
    let _ = pp_expr err_formatter e
    in pp_print_flush err_formatter () 



(* useful for debugging *) 

let string_of_uop = function 
  | NEG -> "NEG" 
  | NOT -> "NOT" 
  | READ -> "READ" 

let string_of_bop = function 
  | ADD -> "ADD" 
  | MUL  -> "MUL" 
  | DIV  -> "DIV" 
  | SUB -> "SUB" 
  | LT   -> "LT" 
  | EQI   -> "EQI" 
  | EQB   -> "EQB" 
  | AND   -> "AND" 
  | OR   -> "OR" 

let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_expr = function
    | Unit _            -> "Unit"
    | Var(_, x)            -> mk_con "Var" [x]
    | Integer(_, n)        -> mk_con "Integer" [string_of_int n]
    | Boolean(_, b)        -> mk_con "Boolean" [string_of_bool b]
    | UnaryOp(_, op, e)   -> mk_con "UnaryOp" [string_of_uop op; string_of_expr e]
    | Op(_, e1, op, e2)   -> mk_con "Op" [string_of_expr e1; string_of_bop op; string_of_expr e2]
    | If(_, e1, e2, e3)   -> mk_con "If" [string_of_expr e1; string_of_expr e2; string_of_expr e3]
    | Pair(_, e1, e2)     -> mk_con "Pair" [string_of_expr e1; string_of_expr e2]
    | Fst(_, e)            -> mk_con "Fst" [string_of_expr e]
    | Snd(_, e)            -> mk_con "Snd" [string_of_expr e]
    | Inl(_, e)            -> mk_con "Inl" [string_of_expr e]
    | Inr(_, e)            -> mk_con "Inr" [string_of_expr e]
    | Lambda(_, x, e)     -> mk_con "Lambda" [x; string_of_expr e]
    | App(_, e1, e2)      -> mk_con "App" [string_of_expr e1; string_of_expr e2]
    | Seq(_, el)           -> mk_con "Seq" [string_of_expr_list el]
    | While(_, e1, e2)   -> mk_con "While" [string_of_expr e1; string_of_expr e2]
    | Ref(_, e)            -> mk_con "Ref" [string_of_expr e]
    | Deref(_, e)          -> mk_con "Deref" [string_of_expr e]
    | Assign (_, e1, e2)  -> mk_con "Assign" [string_of_expr e1; string_of_expr e2]
    | LetFun(_, f, (_, x, e1), e2)      ->
          mk_con "LetFun" [f; mk_con "" [x; string_of_expr e1]; string_of_expr e2]
    | LetRecFun(_, f, (_, x, e1), e2)   ->
          mk_con "LetRecFun" [f; mk_con "" [x; string_of_expr e1]; string_of_expr e2]
    | Case(_, e, (_, x1, e1), (_, x2, e2)) ->
          mk_con "Case" [
              string_of_expr e; 
	      mk_con "" [x1; string_of_expr e1]; 
	      mk_con "" [x2; string_of_expr e2]]

and string_of_expr_list = function 
  | [] -> "" 
  | [e] -> string_of_expr e 
  |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)

let rec map f = function
| Unit a            -> Unit(f a)
| Var(a, x)            -> Var(f a, x)
| Integer(a, n)        -> Integer(f a, n)
| Boolean(a, b)        -> Boolean(f a, b)
| UnaryOp(a, op, e)   -> UnaryOp(f a, op, map f e)
| Op(a, e1, op, e2)   -> Op(f a, map f e1, op, map f e2)
| If(a, e1, e2, e3)   -> If(f a, map f e1, map f e2, map f e3)
| Pair(a, e1, e2)     -> Pair(f a, map f e1, map f e2)
| Fst(a, e)            -> Fst(f a, map f e)
| Snd(a, e)            -> Snd(f a, map f e)
| Inl(a, e)            -> Inl(f a, map f e)
| Inr(a, e)            -> Inr(f a, map f e)
| Lambda(a, x, e)     -> Lambda(f a, x, map f e)
| App(a, e1, e2)      -> App(f a, map f e1, map f e2)
| Seq(a, el)           -> Seq(f a, List.map (map f) el)
| While(a, e1, e2)   -> While(f a, map f e1, map f e2)
| Ref(a, e)            -> Ref(f a, map f e)
| Deref(a, e)          -> Deref(f a, map f e)
| Assign (a, e1, e2)  -> Assign (f a, map f e1, map f e2)
| LetFun(a, f', (a', x, e1), e2)      ->
      LetFun(f a, f', (f a', x, map f e1), map f e2)
| LetRecFun(a, f', (a', x, e1), e2)   ->
      LetRecFun(f a, f', (f a', x, map f e1), map f e2)
| Case(a, e, (a', x1, e1), (a'', x2, e2)) ->
      Case(f a, map f e, (f a', x1, map f e1), (f a'', x2, map f e2))

let get_tag = function
| Unit a            -> a
| Var(a, _)            -> a
| Integer(a, _)        -> a
| Boolean(a, _)        -> a
| UnaryOp(a, _, _)   -> a
| Op(a, _, _, _)   -> a
| If(a, _, _, _)   -> a
| Pair(a, _, _)     -> a
| Fst(a, _)            -> a
| Snd(a, _)            -> a
| Inl(a, _)            -> a
| Inr(a, _)            -> a
| Lambda(a, _, _)     -> a
| App(a, _, _)      -> a
| Seq(a, _)           -> a
| While(a, _, _)   -> a
| Ref(a, _)            -> a
| Deref(a, _)          -> a
| Assign (a, _, _)  -> a
| LetFun(a, _, _, _)      -> a
| LetRecFun(a, _, _, _)   -> a
| Case(a, _, _, _) -> a