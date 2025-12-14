let prime (n : int) : bool =
  if n < 2 then
    false
  else
    let ans : bool ref = ref true in
    let d : int ref = ref 2 in
    begin
      (* !d * !d <= n *)
      while !ans && !d * !d < n + 1 do
        if n / !d * !d = n then
          ans := false
        else
          d := !d + 1;
      !ans
    end
in
prime 101
