type address
type 'a t

val empty : 'a t
val alloc : 'a -> 'a t -> address * 'a t
val get : address -> 'a t -> 'a
val set : address -> 'a -> 'a t -> 'a t
val to_list : 'a t -> (address * 'a) list
val size : 'a t -> int

val pp_binding :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> address * 'a -> unit

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val pp_address : Format.formatter -> address -> unit
