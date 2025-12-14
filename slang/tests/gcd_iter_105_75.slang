let gcd_iter (p : int * int) : int =
  let m : int ref = ref (fst p) in
  let n : int ref = ref (snd p) in
  begin
    while ~(!m = !n) do
      if !m < !n then
        n := !n - !m
      else
        m := !m - !n;
    !m
  end
in
gcd_iter (105, 75)
