type var = string [@@deriving show]

module Unary_op = struct
  type t = Neg | Not [@@deriving show { with_path = false }]
end

module Binary_op = struct
  type t = Add | Sub | Mul | Div | Lt | And | Or | Eq
  [@@deriving show { with_path = false }]
end

module Loc = struct
  type t = { line : int; column : int } [@@deriving show { with_path = false }]

  let of_position (pos : Lexing.position) =
    { line = pos.pos_lnum; column = pos.pos_cnum - pos.pos_bol + 1 }

  let pp_nice ppf { line; column } =
    Format.fprintf ppf "Ln %d, Col %d" line column
end

module Type = struct
  type t =
    | Int
    | Bool
    | Unit
    | Ref of t
    | Arrow of t * t
    | Product of t * t
    | Sum of t * t
  [@@deriving show { with_path = false }]

  open Format

  let rec pp_aux prec ppf =
    let pfprintf (prec' : int) ppf format =
      if prec' < prec then
        fprintf ppf ("(" ^^ format ^^ ")")
      else
        fprintf ppf format
    in
    function
    | Int -> pfprintf 1000 ppf "int"
    | Bool -> pfprintf 1000 ppf "bool"
    | Unit -> pfprintf 1000 ppf "unit"
    | Ref t -> pfprintf 900 ppf "%a ref" (pp_aux 900) t
    | Arrow (t1, t2) ->
        pfprintf 800 ppf "%a -> %a" (pp_aux 801) t1 (pp_aux 800) t2
    | Product (t1, t2) ->
        pfprintf 700 ppf "%a * %a" (pp_aux 700) t1 (pp_aux 701) t2
    | Sum (t1, t2) -> pfprintf 600 ppf "%a + %a" (pp_aux 600) t1 (pp_aux 601) t2

  let pp_nice = pp_aux 0
end

type t = { loc : Loc.t; expr : expr }

and expr =
  | Unit
  | What
  | Var of var [@printer fun ppf -> Format.fprintf ppf "Var %s"]
  | Integer of int [@printer fun ppf -> Format.fprintf ppf "Integer %d"]
  | Boolean of bool [@printer fun ppf -> Format.fprintf ppf "Boolean %b"]
  | UnaryOp of Unary_op.t * t
  | BinaryOp of t * Binary_op.t * t
  | If of t * t * t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Inl of Type.t * t
  | Inr of Type.t * t
  | Case of t * lambda * lambda
  | While of t * t
  | Seq of t list
  | Ref of t
  | Deref of t
  | Assign of t * t
  | Lambda of lambda
  | App of t * t
  | Let of var * Type.t * t * t
  | LetFun of var * lambda * Type.t * t

and lambda = var * Type.t * t [@@deriving show { with_path = false }]

open Format

(** Documentation of Format can be found here:
    http://caml.inria.fr/resources/doc/guides/format.en.html
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html *)

let rec pp_aux (prec : int) (ppf : formatter) t =
  let pfprintf (prec' : int) ppf format =
    if prec' < prec then
      fprintf ppf ("(" ^^ format ^^ ")")
    else
      fprintf ppf format
  in
  match t.expr with
  | Unit -> pfprintf 1000 ppf "()"
  | Pair (e1, e2) ->
      pfprintf 1000 ppf "(@[%a,@ %a@])" (pp_aux 0) e1 (pp_aux 0) e2
  | Var x -> pfprintf 1000 ppf "%s" x
  | Integer n -> pfprintf 1000 ppf "%d" n
  | Boolean b -> pfprintf 1000 ppf "%b" b
  | What -> pfprintf 1000 ppf "?"
  | UnaryOp (Neg, e) -> pfprintf 900 ppf "-%a" (pp_aux 900) e
  | UnaryOp (Not, e) -> pfprintf 900 ppf "~%a" (pp_aux 900) e
  | Deref e -> pfprintf 900 ppf "!%a" (pp_aux 900) e
  | App (e1, e2) -> pfprintf 800 ppf "%a %a" (pp_aux 800) e1 (pp_aux 800) e2
  | Fst e -> pfprintf 800 ppf "fst %a" (pp_aux 800) e
  | Snd e -> pfprintf 800 ppf "snd %a" (pp_aux 800) e
  | Inl (t, e) -> pfprintf 800 ppf "inl %a %a" Type.pp_nice t (pp_aux 800) e
  | Inr (t, e) -> pfprintf 800 ppf "inr %a %a" Type.pp_nice t (pp_aux 800) e
  | Ref e -> pfprintf 800 ppf "ref %a" (pp_aux 800) e
  | BinaryOp (e1, Mul, e2) ->
      pfprintf 750 ppf "%a * %a" (pp_aux 750) e1 (pp_aux 751) e2
  | BinaryOp (e1, Div, e2) ->
      pfprintf 750 ppf "%a / %a" (pp_aux 750) e1 (pp_aux 751) e2
  | BinaryOp (e1, Add, e2) ->
      pfprintf 740 ppf "%a + %a" (pp_aux 740) e1 (pp_aux 741) e2
  | BinaryOp (e1, Sub, e2) ->
      pfprintf 740 ppf "%a - %a" (pp_aux 740) e1 (pp_aux 741) e2
  | BinaryOp (e1, Lt, e2) ->
      pfprintf 730 ppf "%a < %a" (pp_aux 730) e1 (pp_aux 731) e2
  | BinaryOp (e1, And, e2) ->
      pfprintf 720 ppf "%a && %a" (pp_aux 721) e1 (pp_aux 720) e2
  | BinaryOp (e1, Or, e2) ->
      pfprintf 710 ppf "%a || %a" (pp_aux 711) e1 (pp_aux 710) e2
  | BinaryOp (e1, Eq, e2) ->
      pfprintf 700 ppf "%a = %a" (pp_aux 700) e1 (pp_aux 701) e2
  | If (e1, e2, e3) ->
      pfprintf 100 ppf
        "@[<v>@[<hv>if@;<1 2>%a@ then@]@;<1 2>%a@ else@;<1 2>@[%a@]@]"
        (pp_aux 100) e1 (pp_aux 0) e2 (pp_aux 0) e3
  | Case (e, (x1, t1, e1), (x2, t2, e2)) ->
      pfprintf 100 ppf
        "@[<v 2>case %a of@\n\
         | inl %s : %a ->@;\
         <1 4>%a @\n\
         | inr %s : %a ->@;\
         <1 4>%a@]"
        (pp_aux 0) e x1 Type.pp_nice t1 (pp_aux 0) e1 x2 Type.pp_nice t2
        (pp_aux 0) e2
  | Seq es ->
      pfprintf 100 ppf "@[<v>%a@]"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ") (pp_aux 100))
        es
  | While (e1, e2) ->
      pfprintf 100 ppf "@[<v>@[<hv>while@;<1 2>%a@ do@]@;<1 2>%a@]" (pp_aux 100)
        e1 (pp_aux 100) e2
  | Assign (e1, e2) ->
      pfprintf 200 ppf "%a := %a" (pp_aux 201) e1 (pp_aux 200) e2
  | Lambda (x, t, e) ->
      pfprintf 100 ppf "@[fun %s : %a ->@;<1 2>@[%a@]@]" x Type.pp_nice t
        (pp_aux 100) e
  | Let (x, t, e1, e2) ->
      pfprintf 100 ppf "@[<hv>let %s : %a =@;<1 2>%a@ in@\n%a@]" x Type.pp_nice
        t (pp_aux 100) e1 (pp_aux 100) e2
  | LetFun (f, (x, t1, e1), t2, e2) ->
      pfprintf 100 ppf "@[<hv>let %s (%s : %a) : %a =@;<1 2>%a@ in@\n%a@]" f x
        Type.pp_nice t1 Type.pp_nice t2 (pp_aux 100) e1 (pp_aux 100) e2

let pp_nice = pp_aux 0
