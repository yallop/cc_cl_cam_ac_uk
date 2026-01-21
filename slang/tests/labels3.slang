let f (x : int) : int =
  if x = 0 then
    1
  else
    x * f (x - 1)
in
let f (y : int * int) : int =
  if fst y = 0 then
    1
  else
    fst y * f (fst y - 1, 0)
in
f (3, 0)
