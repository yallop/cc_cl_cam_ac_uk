type 'f t =
  | Ref of Heap.address
  | Int of int
  | Bool of bool
  | Unit
  | Pair of 'f t * 'f t
  | Inl of 'f t
  | Inr of 'f t
  | Fun of 'f

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
