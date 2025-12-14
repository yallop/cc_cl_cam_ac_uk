

let f (y : int) : int -> int =
  let g (x : int) : int = 3 + y + x in
  g
in
let add21 : int -> int = f 21 in
let add17 : int -> int = f 17 in
add17 3 + add21 10
