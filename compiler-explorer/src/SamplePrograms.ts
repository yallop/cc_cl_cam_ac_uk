const samplePrograms = {
  fib: `let fib( m : int) : int =
    if m = 0
    then 1
    else if m = 1
        then 1
        else fib (m - 1) + fib (m -2)
in
    fib(1)`,

  fun: `(* the reason we need closures on the heap ... *)

let f(y : int) : int -> int =
  let g(x :int) : int = y + x in g
in
  let add21 : int -> int  = f(21) in
      let add17 : int -> int  = f(17) in
         add17(3) + add21(60)`,

  gcd: `let gcd(p : int * int) : int =
    let m : int = fst p in
       let  n : int = snd p in
          if m = n
          then m
          else if m < n
               then gcd(m, n - m)
               else gcd(m - n, n)
in
   let x : int = 10 in
      let  y : int = 2 in gcd(x, y)`
};

export default samplePrograms;
