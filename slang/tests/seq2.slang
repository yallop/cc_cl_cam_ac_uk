
let x : (int -> int) ref = ref (fun (z : int) -> z + 17) in
let y : int ref = ref 0 in
begin
  y := !x 4;
  x := (fun (w : int) -> w * 10);
  !x 8 + !y
end
