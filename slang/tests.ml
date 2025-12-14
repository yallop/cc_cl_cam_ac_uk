let test_dir = "slang/tests"

let manifest : (string * string option) list =
  List.map
    (fun (file, expected) -> (Filename.concat test_dir file, Some expected))
    [
      ("fib5_v2.slang", "8");
      ("fib_rec_10.slang", "55");
      ("fib_iter_42.slang", "267914296");
      ("collatz_rec_27.slang", "111");
      ("collatz_iter_27.slang", "111");
      ("fact_rec_10.slang", "3628800");
      ("fact_iter_10.slang", "3628800");
      ("gcd_rec_105_75.slang", "15");
      ("gcd_iter_105_75.slang", "15");
      ("prime_rec_49.slang", "false");
      ("prime_rec_101.slang", "true");
      ("prime_iter_49.slang", "false");
      ("prime_iter_101.slang", "true");
      ("church.slang", "32");
      ("poly_eq.slang", "42");
      ("bool.slang", "false");
      ("bidmas.slang", "4");
      ("add3.slang", "101");
      ("add4.slang", "101");
      ("add5.slang", "101");
      ("add6.slang", "101");
      ("add7.slang", "101");
      ("expr.slang", "101");
      ("expr2.slang", "101");
      ("let1.slang", "101");
      ("nest.slang", "7");
      ("nest2.slang", "42");
      ("stack.slang", "32");
      ("stack2.slang", "17");
      ("alpha.slang", "72");
      ("fun.slang", "101");
      ("labels2.slang", "1");
      ("labels3.slang", "6");
      ("lambda1.slang", "101");
      ("lambda2.slang", "101");
      ("lambda3.slang", "101");
      ("seq0.slang", "101");
      ("seq1.slang", "101");
      ("seq2.slang", "101");
      ("ref1.slang", "101");
      ("ref2.slang", "101");
      ("ref3.slang", "101");
      ("while.slang", "3");
    ]
