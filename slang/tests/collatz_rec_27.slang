let collatz (n : int) : int =
  if n = 1 then
    0
  else if n / 2 * 2 = n then
    (* n is even because "/" is integer division *)
    1 + collatz (n / 2)
  else
    (* n is odd *)
    1 + collatz (3 * n + 1)
in
collatz 27
