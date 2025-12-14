let collatz (n : int) : int =
  let ans : int ref = ref 0 in
  let n : int ref = ref n in (* Variable shadowing *)
  begin
    while 1 < !n do begin
      if !n / 2 * 2 = !n then
        (* n is even because "/" is integer division *)
        n := !n / 2
      else
        (* n is odd *)
        n := 3 * !n + 1;
      ans := !ans + 1
    end;
    !ans
  end
in
collatz 27
