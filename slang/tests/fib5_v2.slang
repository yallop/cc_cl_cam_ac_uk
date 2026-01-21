let h (p : int * int) : int  =
  let fib (m : int) : int =
    if m = 0 then
      snd p
    else if m = 1 then
      snd p
    else
      fib (m - 1) + fib (m - 2)
  in
  fib (fst p)
in
  h (5, 1)
