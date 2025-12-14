type var = string

val pp_var : Format.formatter -> var -> unit

module Unary_op : sig
  type t = Neg | Not | Read

  val to_fun : t -> 'a Value.t -> 'a Value.t
  val pp : Format.formatter -> t -> unit
end

module Binary_op : sig
  type t = Add | Sub | Mul | Div | Lt | And | Or | Eqi | Eqb

  val to_fun : t -> 'a Value.t * 'a Value.t -> 'a Value.t
  val pp : Format.formatter -> t -> unit
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

and lambda = var * t

val pp : Format.formatter -> t -> unit
val pp_nice : Format.formatter -> t -> unit
