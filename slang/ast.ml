type var = string [@@deriving show]

module Unary_op = struct
  type t = Neg | Not | Read [@@deriving show { with_path = false }]

  let to_fun : t -> 'a Value.t -> 'a Value.t = function
    | Neg -> ( function Int i -> Int (-i) | _ -> failwith "")
    | Not -> ( function Bool i -> Bool (not i) | _ -> failwith "")
    | Read -> (
        function
        | Unit ->
            print_string ">>> ";
            Int (read_int ())
        | _ -> failwith "")
end

module Binary_op = struct
  type t = Add | Sub | Mul | Div | Lt | And | Or | Eqi | Eqb
  [@@deriving show { with_path = false }]

  let to_fun : t -> 'a Value.t * 'a Value.t -> 'a Value.t = function
    | Add -> ( function Int m, Int n -> Int (m + n) | _ -> failwith "")
    | Sub -> ( function Int m, Int n -> Int (m - n) | _ -> failwith "")
    | Mul -> ( function Int m, Int n -> Int (m * n) | _ -> failwith "")
    | Div -> ( function Int m, Int n -> Int (m / n) | _ -> failwith "")
    | Lt -> ( function Int m, Int n -> Bool (m < n) | _ -> failwith "")
    | And -> ( function Bool m, Bool n -> Bool (m && n) | _ -> failwith "")
    | Or -> ( function Bool m, Bool n -> Bool (m || n) | _ -> failwith "")
    | Eqi -> ( function Int m, Int n -> Bool (m = n) | _ -> failwith "")
    | Eqb -> ( function Bool m, Bool n -> Bool (m = n) | _ -> failwith "")
end

type t =
  | Unit
  | Var of var
  | Integer of int
  | Boolean of bool
  | UnaryOp of Unary_op.t * t
  | BinaryOp of t * Binary_op.t * t
  | If of t * t * t
  | Pair of t * t
  | Fst of t
  | Snd of t
  | Inl of t
  | Inr of t
  | Case of t * lambda * lambda
  | While of t * t
  | Seq of t list
  | Ref of t
  | Deref of t
  | Assign of t * t
  | Lambda of lambda
  | App of t * t
  | LetFun of var * lambda * t
  | LetRecFun of var * lambda * t
[@@deriving show { with_path = false }]

and lambda = var * t

open Format

(** Documentation of Format can be found here:
    http://caml.inria.fr/resources/doc/guides/format.en.html
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html *)

let rec pp_aux (prec : int) (ppf : formatter) =
  let pfprintf (prec' : int) ppf format =
    if prec' < prec then
      fprintf ppf ("(" ^^ format ^^ ")")
    else
      fprintf ppf format
  in
  function
  | Unit -> pfprintf 1000 ppf "()"
  | Pair (e1, e2) ->
      pfprintf 1000 ppf "(@[%a,@ %a@])" (pp_aux 0) e1 (pp_aux 0) e2
  | Var x -> pfprintf 1000 ppf "%s" x
  | Integer n -> pfprintf 1000 ppf "%d" n
  | Boolean b -> pfprintf 1000 ppf "%b" b
  | UnaryOp (Neg, e) -> pfprintf 900 ppf "-%a" (pp_aux 900) e
  | UnaryOp (Not, e) -> pfprintf 900 ppf "~%a" (pp_aux 900) e
  | UnaryOp (Read, e) -> pfprintf 900 ppf "?%a" (pp_aux 900) e
  | Deref e -> pfprintf 900 ppf "!%a" (pp_aux 900) e
  | App (e1, e2) -> pfprintf 800 ppf "%a %a" (pp_aux 800) e1 (pp_aux 800) e2
  | Fst e -> pfprintf 800 ppf "fst %a" (pp_aux 800) e
  | Snd e -> pfprintf 800 ppf "snd %a" (pp_aux 800) e
  | Inl e -> pfprintf 800 ppf "inl %a" (pp_aux 800) e
  | Inr e -> pfprintf 800 ppf "inr %a" (pp_aux 800) e
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
  | BinaryOp (e1, Eqi, e2) ->
      pfprintf 700 ppf "%a = %a" (pp_aux 700) e1 (pp_aux 701) e2
  | BinaryOp (e1, Eqb, e2) ->
      pfprintf 700 ppf "%a = %a" (pp_aux 700) e1 (pp_aux 701) e2
  | If (e1, e2, e3) ->
      pfprintf 100 ppf
        "@[<v>@[<hv>if@;<1 2>%a@ then@]@;<1 2>%a@ else@;<1 2>@[%a@]@]"
        (pp_aux 100) e1 (pp_aux 0) e2 (pp_aux 0) e3
  | Case (e, (x1, e1), (x2, e2)) ->
      pfprintf 100 ppf
        "@[<v 2>case %a of@\n| inl %s ->@;<1 4>%a @\n| inr %s ->@;<1 4>%a@]"
        (pp_aux 0) e x1 (pp_aux 0) e1 x2 (pp_aux 0) e2
  | Seq es ->
      pfprintf 100 ppf "@[<v>%a@]"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ") (pp_aux 100))
        es
  | While (e1, e2) ->
      pfprintf 100 ppf "@[<v>@[<hv>while@;<1 2>%a@ do@]@;<1 2>%a@]" (pp_aux 100)
        e1 (pp_aux 100) e2
  | Assign (e1, e2) ->
      pfprintf 200 ppf "%a := %a" (pp_aux 201) e1 (pp_aux 200) e2
  | Lambda (x, e) ->
      pfprintf 100 ppf "@[fun %s ->@;<1 2>@[%a@]@]" x (pp_aux 100) e
  | LetFun (f, (x, e1), e2) ->
      pfprintf 100 ppf "@[<hv>let %s %s =@;<1 2>%a@ in@\n%a@]" f x (pp_aux 100)
        e1 (pp_aux 100) e2
  | LetRecFun (f, (x, e1), e2) ->
      pfprintf 100 ppf "@[<hv>let rec %s %s =@;<1 2>%a@ in@\n%a@]" f x
        (pp_aux 100) e1 (pp_aux 100) e2

let pp_nice = pp_aux 0
