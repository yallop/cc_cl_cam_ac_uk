let prime (n : int) : bool =
  let check (d : int) : bool =
    if n < d * d then
      true
    else if n / d * d = n then
      false
    else
      check (d + 1)
  in
  if n < 2 then
    false
  else
    check 2
in
prime 101
