(**************************************
Compiler Construction 2016
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)

let rec fib (m : int) : int =
  if m = 0 then
    0
  else if m = 1 then
    1
  else
    fib (m - 1) + fib (m - 2)

(** Note : fib_cps (m, f) = f (fib m) *)
let rec fib_cps (m, cnt) : int =
  if m = 0 then
    cnt 0
  else if m = 1 then
    cnt 1
  else
    fib_cps (m - 1, fun a -> fib_cps (m - 2, fun b -> cnt (a + b)))

(** The initial continuation *)
let id (x : int) : int = x

(** note : fib_1 m = fib_cps (m, id) = id (fib m) = fib m *)
let fib_1 (m : int) : int = fib_cps (m, id)
