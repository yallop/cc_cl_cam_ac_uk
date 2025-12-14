type var = string

module Unary_op : sig
  type t = Neg | Not

  val pp : Format.formatter -> t -> unit
end

module Binary_op : sig
  type t = Add | Sub | Mul | Div | Lt | And | Or | Eq

  val pp : Format.formatter -> t -> unit
end

module Loc : sig
  type t = { line : int; column : int }

  val of_position : Lexing.position -> t
  val pp : Format.formatter -> t -> unit
  val pp_nice : Format.formatter -> t -> unit
end

module Type : sig
  type t =
    | Int
    | Bool
    | Unit
    | Ref of t
    | Arrow of t * t
    | Product of t * t
    | Sum of t * t

  val pp : Format.formatter -> t -> unit
  val pp_nice : Format.formatter -> t -> unit
end

type t = { loc : Loc.t; expr : expr }

and expr =
  | Unit
  | What
  | Var of var
  | Integer of int
  | Boolean of bool
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

and lambda = var * Type.t * t

val pp : Format.formatter -> t -> unit
val pp_nice : Format.formatter -> t -> unit
