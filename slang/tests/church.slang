(* A real test of closures. Type inference would make this a lot more concise. *)

let succ (cn : (int -> int) -> int -> int) : (int -> int) -> int -> int =
  fun (f : int -> int) ->
    fun (x : int) ->
      f (cn f x)
in
let church (n : int) : (int -> int) -> int -> int =
  if n = 0 then
    fun (f : int -> int) ->
      fun (x : int) -> x
  else
    succ (church (n - 1))
in
let add :
    ((int -> int) -> int -> int) ->
    ((int -> int) -> int -> int) ->
    ((int -> int) -> int -> int) =
  fun (cn : (int -> int) -> int -> int) ->
    fun (cm : (int -> int) -> int -> int) ->
      fun (f : int -> int) ->
        fun (x : int) ->
          cn f (cm f x)
in
let c5 : (int -> int) -> int -> int =
  add (church 2) (church 3)
in
let double (x : int) : int = x + x in
(* double (double (double (double (double 1)))) *)
c5 double 1
