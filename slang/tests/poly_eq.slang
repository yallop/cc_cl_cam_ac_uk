(* Test that "=" correctly monomorphises to integer or boolean equality *)

if 2 + 2 = 5 || true = true then
  if 2 + 2 = 4 && false = false then
    42
  else
    -2
else
  -1
