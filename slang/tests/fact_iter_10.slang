let fact (n : int) : int =
  let ans : int ref = ref 1 in
  let n : int ref = ref n in
  begin
    while 0 < !n do begin
      ans := !ans * !n;
      n := !n - 1
    end;
    !ans
  end
in
fact 10
