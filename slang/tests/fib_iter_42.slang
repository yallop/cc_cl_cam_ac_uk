let fib (n : int) : int =
  let ab : (int * int) ref = ref (0, 1) in
  let n : int ref = ref n in
  begin
    while 0 < !n do begin
      ab := (snd !ab, fst !ab + snd !ab);
      n := !n - 1
    end;
    fst !ab
  end
in
fib 42 (* Recursive fib could never... *)
