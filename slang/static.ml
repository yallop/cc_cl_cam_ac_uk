open Past
open Errors

type env = (Past.var * Past.type_expr) list

let internal_error msg = complainf "INTERNAL ERROR: %s" msg

let report_expecting e msg t =
  let loc = loc_of_expr e in
  complainf "ERROR at location %s\nExpression %s\nhas type %s, but expecting %s"
    (string_of_loc loc) (string_of_expr e) (string_of_type t) msg

let report_types_not_equal loc t1 t2 =
  complainf "Error near location %s\nExpecting type %s to be equal to type %s"
    (string_of_loc loc) (string_of_type t1) (string_of_type t2)

let report_type_mismatch (e1, t1) (e2, t2) =
  let loc1 = loc_of_expr e1 and loc2 = loc_of_expr e2 in
  complainf
    "ERROR, Type Mismatch: expecting equal types, however\n\
     at location %s\n\
     expression %s\n\
     has type %s and at location %s\n\
     expression %s\n\
     has type %s"
    (string_of_loc loc1) (string_of_expr e1) (string_of_type t1)
    (string_of_loc loc2) (string_of_expr e2) (string_of_type t2)

let rec find loc x = function
  | [] -> complainf "%s is not defined at %s" x (string_of_loc loc)
  | (y, v) :: _ when x = y -> v
  | _ :: rest -> find loc x rest

(* may want to make this more interesting someday ... *)
let ty_eq t1 t2 = t1 = t2
let make_pair loc (e1, t1) (e2, t2) = (Pair (loc, e1, e2), TEproduct (t1, t2))
let make_inl loc t2 (e, t1) = (Inl (loc, t2, e), TEunion (t1, t2))
let make_inr loc t1 (e, t2) = (Inr (loc, t1, e), TEunion (t1, t2))
let make_lambda loc x t1 (e, t2) = (Lambda (loc, (x, t1, e)), TEarrow (t1, t2))
let make_ref loc (e, t) = (Ref (loc, e), TEref t)

let make_letfun loc f x t1 t2 (body, t2') (e, t) =
  match ty_eq t2 t2' with
  | true -> (LetFun (loc, f, (x, t1, body), t2, e), t)
  | false -> report_expecting body (string_of_type t2) t2'

let make_letrecfun loc f x t1 t2 (body, t2') (e, t) =
  match ty_eq t2 t2' with
  | true -> (LetRecFun (loc, f, (x, t1, body), t2, e), t)
  | false -> report_expecting body (string_of_type t2) t2'

let make_let loc x t (e1, t1) (e2, t2) =
  if ty_eq t t1 then
    (Let (loc, x, t, e1, e2), t2)
  else
    report_types_not_equal loc t t1

let make_if loc (e1, t1) (e2, t2) (e3, t3) =
  match t1 with
  | TEbool when ty_eq t2 t3 -> (If (loc, e1, e2, e3), t2)
  | TEbool -> report_type_mismatch (e2, t2) (e3, t3)
  | ty -> report_expecting e1 "boolean" ty

let make_app loc (e1, t1) (e2, t2) =
  match t1 with
  | TEarrow (t3, t4) when ty_eq t2 t3 -> (App (loc, e1, e2), t4)
  | TEarrow (t3, _) -> report_expecting e2 (string_of_type t3) t2
  | _ -> report_expecting e1 "function type" t1

let make_fst loc = function
  | e, TEproduct (t, _) -> (Fst (loc, e), t)
  | e, t -> report_expecting e "product" t

let make_snd loc = function
  | e, TEproduct (_, t) -> (Snd (loc, e), t)
  | e, t -> report_expecting e "product" t

let make_deref loc = function
  | e, TEref t' -> (Deref (loc, e), t')
  | e, t -> report_expecting e "ref type" t

let make_uop loc uop (e, t) =
  match (uop, t) with
  | NEG, TEint -> (UnaryOp (loc, uop, e), t)
  | NEG, _ -> report_expecting e "integer" t
  | NOT, TEbool -> (UnaryOp (loc, uop, e), t)
  | NOT, _ -> report_expecting e "boolean" t

let make_bop loc bop (e1, t1) (e2, t2) =
  match (bop, t1, t2) with
  | LT, TEint, TEint -> (Op (loc, e1, bop, e2), TEbool)
  | LT, TEint, t -> report_expecting e2 "integer" t
  | LT, t, _ -> report_expecting e1 "integer" t
  | ADD, TEint, TEint -> (Op (loc, e1, bop, e2), t1)
  | ADD, TEint, t -> report_expecting e2 "integer" t
  | ADD, t, _ -> report_expecting e1 "integer" t
  | SUB, TEint, TEint -> (Op (loc, e1, bop, e2), t1)
  | SUB, TEint, t -> report_expecting e2 "integer" t
  | SUB, t, _ -> report_expecting e1 "integer" t
  | MUL, TEint, TEint -> (Op (loc, e1, bop, e2), t1)
  | MUL, TEint, t -> report_expecting e2 "integer" t
  | MUL, t, _ -> report_expecting e1 "integer" t
  | DIV, TEint, TEint -> (Op (loc, e1, bop, e2), t1)
  | DIV, TEint, t -> report_expecting e2 "integer" t
  | DIV, t, _ -> report_expecting e1 "integer" t
  | OR, TEbool, TEbool -> (Op (loc, e1, bop, e2), t1)
  | OR, TEbool, t -> report_expecting e2 "boolean" t
  | OR, t, _ -> report_expecting e1 "boolean" t
  | AND, TEbool, TEbool -> (Op (loc, e1, bop, e2), t1)
  | AND, TEbool, t -> report_expecting e2 "boolean" t
  | AND, t, _ -> report_expecting e1 "boolean" t
  | EQ, TEbool, TEbool -> (Op (loc, e1, EQB, e2), t1)
  | EQ, TEint, TEint -> (Op (loc, e1, EQI, e2), TEbool)
  | EQ, _, _ -> report_type_mismatch (e1, t1) (e2, t2)
  | EQI, _, _ -> internal_error "EQI found in parsed AST"
  | EQB, _, _ -> internal_error "EQB found in parsed AST"

let make_while loc (e1, t1) (e2, t2) =
  match (t1, t2) with
  | TEbool, TEunit -> (While (loc, e1, e2), TEunit)
  | TEbool, _ -> report_expecting e2 "unit type" t2
  | _ -> report_expecting e1 "boolean" t1

let make_assign loc (e1, t1) (e2, t2) =
  match t1 with
  | TEref t when ty_eq t t2 -> (Assign (loc, e1, e2), TEunit)
  | TEref t -> report_type_mismatch (e1, t) (e2, t2)
  | t -> report_expecting e1 "ref type" t

let make_case loc tl tr x1 x2 (e1, t1) (e2, t2) (e3, t3) =
  match t1 with
  | TEunion (tl', _) when not (ty_eq tl tl') ->
      report_types_not_equal loc tl tl'
  | TEunion (_, tr') when not (ty_eq tr tr') ->
      report_types_not_equal loc tr tr'
  | TEunion (_, _) when not (ty_eq t3 t2) ->
      report_type_mismatch (e2, t2) (e3, t3)
  | TEunion (_, _) -> (Case (loc, e1, (x1, tl, e2), (x2, tr, e3)), t2)
  | t -> report_expecting e1 "disjoint union" t

let rec fv bound : expr -> var list = function
  | Unit _ -> []
  | What _ -> []
  | Integer _ -> []
  | Boolean _ -> []
  | Var (_, x) when List.mem x bound -> []
  | Var (_, x) -> [ x ]
  | Seq (_, el) -> List.concat_map (fv bound) el
  | Ref (_, e)
  | Deref (_, e)
  | UnaryOp (_, _, e)
  | Fst (_, e)
  | Snd (_, e)
  | Inl (_, _, e)
  | Inr (_, _, e) ->
      fv bound e
  | While (_, e1, e2)
  | Assign (_, e1, e2)
  | Op (_, e1, _, e2)
  | Pair (_, e1, e2)
  | App (_, e1, e2) ->
      fv bound e1 @ fv bound e2
  | If (_, e1, e2, e3) -> fv bound e1 @ fv bound e2 @ fv bound e3
  | Case (_, e, (x, _, e1), (y, _, e2)) ->
      fv bound e @ fv (x :: bound) e1 @ fv (y :: bound) e2
  | Lambda (_, (x, _, e)) -> fv (x :: bound) e
  | Let (_, x, _, e1, e2) -> fv bound e1 @ fv (x :: bound) e2
  | LetFun (_, f, (x, _, body), _, e) ->
      (* Functions are implicitly recursive *)
      fv (f :: x :: bound) body @ fv (f :: bound) e
  | LetRecFun _ -> internal_error "LetRecFun found in parsed AST"

let rec elaborate env e =
  match e with
  | Unit _ -> (e, TEunit)
  | What _ -> (e, TEint)
  | Integer _ -> (e, TEint)
  | Boolean _ -> (e, TEbool)
  | Var (loc, x) -> (e, find loc x env)
  | Seq (loc, el) -> elaborate_seq loc env el
  | While (loc, e1, e2) -> make_while loc (elaborate env e1) (elaborate env e2)
  | Ref (loc, e) -> make_ref loc (elaborate env e)
  | Deref (loc, e) -> make_deref loc (elaborate env e)
  | Assign (loc, e1, e2) ->
      make_assign loc (elaborate env e1) (elaborate env e2)
  | UnaryOp (loc, uop, e) -> make_uop loc uop (elaborate env e)
  | Op (loc, e1, bop, e2) ->
      make_bop loc bop (elaborate env e1) (elaborate env e2)
  | If (loc, e1, e2, e3) ->
      make_if loc (elaborate env e1) (elaborate env e2) (elaborate env e3)
  | Pair (loc, e1, e2) -> make_pair loc (elaborate env e1) (elaborate env e2)
  | Fst (loc, e) -> make_fst loc (elaborate env e)
  | Snd (loc, e) -> make_snd loc (elaborate env e)
  | Inl (loc, t, e) -> make_inl loc t (elaborate env e)
  | Inr (loc, t, e) -> make_inr loc t (elaborate env e)
  | Case (loc, e, (x, s, e1), (y, t, e2)) ->
      make_case loc s t x y (elaborate env e)
        (elaborate ((x, s) :: env) e1)
        (elaborate ((y, t) :: env) e2)
  | Lambda (loc, (x, t, e)) -> make_lambda loc x t (elaborate ((x, t) :: env) e)
  | App (loc, e1, e2) -> make_app loc (elaborate env e1) (elaborate env e2)
  | Let (loc, x, t, e1, e2) ->
      make_let loc x t (elaborate env e1) (elaborate ((x, t) :: env) e2)
  | LetFun (loc, f, (x, t1, body), t2, e) ->
      let env' = (f, TEarrow (t1, t2)) :: env in
      let body' = elaborate ((x, t1) :: env') body in
      let bound_vars_except_f =
        x
        :: List.filter_map
             (fun (z, _) ->
               if z = f then
                 None
               else
                 Some x)
             env
      in
      let make =
        if List.mem f (fv bound_vars_except_f body) then
          make_letrecfun
        else
          make_letfun
      in
      make loc f x t1 t2 body' (elaborate env' e)
  | LetRecFun _ -> internal_error "LetRecFun found in parsed AST"

and elaborate_seq loc env el =
  let rec aux carry = function
    | [] -> internal_error "empty sequence found in parsed AST"
    | [ e ] ->
        let e', t = elaborate env e in
        (Seq (loc, List.rev (e' :: carry)), t)
    | e :: rest ->
        let e', _ = elaborate env e in
        aux (e' :: carry) rest
  in
  aux [] el

let infer = elaborate
