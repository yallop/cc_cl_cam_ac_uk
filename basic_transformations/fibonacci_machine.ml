(* We are going to use some of the basic
   transformation described in this directory to
   derive the Fibonacci Machine!

   First read these files

       cps.ml   : continuation-passing-style (cps)
       dfc.ml   : defunctionalization (dfc)
       tail.ml  : tail calls, replace recursion with iteration

   We will eliminate recursion from fib by
   1) transform fib with cps
   2) dfc results
   3) see that resulting continuations represent lists (stacks really)
   4) rewrite code using lists (stacks really)

   The result in Fibonacci Machine computes using iteration,
   step-by-step, and so OCaml should compile this into code
   that is iterative (not using OCaml's runtime stack).
   However, the Fibonacci Machine carries its own stack!

   The easiest way to use this file is to enter the OCaml read-eval print loop
   and type
       #use "fibonacci_machine.ml";;

*)

(** Start with the standard definition *)
let rec fib m =
  if m = 0 then
    0
  else if m = 1 then
    1
  else
    fib (m - 1) + fib (m - 2)

(* Now, apply cps transform. *)
let rec fib_cps (m, cnt) : int =
  if m = 0 then
    cnt 0
  else if m = 1 then
    cnt 1
  else
    fib_cps (m - 1, fun a -> fib_cps (m - 2, fun b -> cnt (a + b)))

(* Here is a version using lets, not "lambdas" *)
let rec fib_cps_v2 (m, cnt) =
  if m = 0 then
    cnt 0
  else if m = 1 then
    cnt 1
  else
    let cnt2 a b = cnt (a + b) in
    let cnt1 a = fib_cps_v2 (m - 2, cnt2 a) in
    fib_cps_v2 (m - 1, cnt1)

let id (x : int) : int = x
let fib_1 (m : int) : int = fib_cps_v2 (m, id)

(* Now apply defunctionalization (dfc). *)

(* datatype to represent continuations *)
type cnt = ID | CNT1 of int * cnt | CNT2 of int * cnt

let rec apply_cnt : cnt * int -> int = function
  | ID, a -> a
  | CNT1 (m, cnt), a -> fib_cps_dfc (m - 2, CNT2 (a, cnt))
  | CNT2 (a, cnt), b -> apply_cnt (cnt, a + b)

and fib_cps_dfc (m, cnt) : int =
  if m = 0 then
    apply_cnt (cnt, 0)
  else if m = 1 then
    apply_cnt (cnt, 1)
  else
    fib_cps_dfc (m - 1, CNT1 (m, cnt))

(*  fib_2 : int -> int *)
let fib_2 m = fib_cps_dfc (m, ID)

(* A Eureka moment. Look closely at the definition of the type cnt.
   These are just lists with ID = [] and two flavours of "cons"!
   we can replace this type with a list of tagged integers.

   I'll give the tags suggestive names.
*)

type tag = SUB2 of int | PLUS of int [@@deriving show { with_path = false }]
type tag_list_cnt = tag list

let rec apply_tag_list_cnt : tag_list_cnt * int -> int = function
  | [], a -> a
  | SUB2 m :: cnt, a -> fib_cps_dfc_tags (m - 2, PLUS a :: cnt)
  | PLUS a :: cnt, b -> apply_tag_list_cnt (cnt, a + b)

and fib_cps_dfc_tags (m, cnt) : int =
  if m = 0 then
    apply_tag_list_cnt (cnt, 0)
  else if m = 1 then
    apply_tag_list_cnt (cnt, 1)
  else
    fib_cps_dfc_tags (m - 1, SUB2 m :: cnt)

let fib_3 (m : int) : int = fib_cps_dfc_tags (m, [])

(* Another Eureka moment. Look closely at this code.

   The function apply_tag_list_cnt and fib_cps_dfc_tags
   are defined with mutual recursion. However, they are
   collectively tail-recursive as well. Can we combine
   them into a single tail-recursive function?  Yes.
   We are really defining a
   state-transition system  with two distinct state types
   (one for transitions that start in fib_cps_dfc_tags
   and one for transitions that start in apply_tag_list_cnt.)

   I'll give these states suggestive names.
*)

type state_type =
  | SUB1 (* for right-hand-sides starting with fib_   *)
  | APPL (* for right-hand-sides starting with apply_ *)
[@@deriving show { with_path = false }]

type state = state_type * int * tag_list_cnt

(* We now rewrite the version above as a state-transition evaluator. *)
let rec eval : state -> int = function
  | SUB1, 0, cnt -> eval (APPL, 0, cnt)
  | SUB1, 1, cnt -> eval (APPL, 1, cnt)
  | SUB1, m, cnt -> eval (SUB1, m - 1, SUB2 m :: cnt)
  | APPL, a, SUB2 m :: cnt -> eval (SUB1, m - 2, PLUS a :: cnt)
  | APPL, b, PLUS a :: cnt -> eval (APPL, a + b, cnt)
  | APPL, a, [] -> a

let fib_4 (m : int) : int = eval (SUB1, m, [])

(* Finally, The Fibonacci Machine!

   Since eval is now tail-recursive, I'll rewrite it to
   make this very explicit (as a step function and an eval function).

   I'll add some pretty-printing at this point so that
   we can watch our machine grind out Fibonacci numbers.

*)

let step : state -> state = function
  | SUB1, 0, cnt -> (APPL, 0, cnt)
  | SUB1, 1, cnt -> (APPL, 1, cnt)
  | SUB1, m, cnt -> (SUB1, m - 1, SUB2 m :: cnt)
  | APPL, a, SUB2 m :: cnt -> (SUB1, m - 2, PLUS a :: cnt)
  | APPL, b, PLUS a :: cnt -> (APPL, a + b, cnt)
  | _ -> failwith "step : runtime error!"

let print_state n (t, m, cnt) =
  Format.printf "%d %a || %d || %s\n" n pp_state_type t m
    ([%show: tag list] (List.rev cnt))

(** Set to false if you don't want to watch the machine's transitions ... *)
let verbose = ref true

(** The OCaml compiler probably compiles this recursive function into iteration
    (no stack). Of course we have build "the stack" into our machine ;-) *)
let rec eval_steps (n : int) (state : state) : int =
  if !verbose then
    print_state n state;
  match state with APPL, a, [] -> a | _ -> eval_steps (n + 1) (step state)

let fib_5 (m : int) : int = eval_steps 1 (SUB1, m, [])

(** Just for testing to see if all versions return the same value *)
let fibs (m : int) = [ fib m; fib_1 m; fib_2 m; fib_3 m; fib_4 m; fib_5 m ]

let _ = List.iter print_int (fibs 6)

(* Here is a trace of fib_5 6.

fib_5 6;;

 1 SUB1 || 6 || []
 2 SUB1 || 5 || [SUB2 6]
 3 SUB1 || 4 || [SUB2 6, SUB2 5]
 4 SUB1 || 3 || [SUB2 6, SUB2 5, SUB2 4]
 5 SUB1 || 2 || [SUB2 6, SUB2 5, SUB2 4, SUB2 3]
 6 SUB1 || 1 || [SUB2 6, SUB2 5, SUB2 4, SUB2 3, SUB2 2]
 7 APPL || 1 || [SUB2 6, SUB2 5, SUB2 4, SUB2 3, SUB2 2]
 8 SUB1 || 0 || [SUB2 6, SUB2 5, SUB2 4, SUB2 3, PLUS 1]
 9 APPL || 1 || [SUB2 6, SUB2 5, SUB2 4, SUB2 3, PLUS 1]
10 APPL || 2 || [SUB2 6, SUB2 5, SUB2 4, SUB2 3]
11 SUB1 || 1 || [SUB2 6, SUB2 5, SUB2 4, PLUS 2]
12 APPL || 1 || [SUB2 6, SUB2 5, SUB2 4, PLUS 2]
13 APPL || 3 || [SUB2 6, SUB2 5, SUB2 4]
14 SUB1 || 2 || [SUB2 6, SUB2 5, PLUS 3]
15 SUB1 || 1 || [SUB2 6, SUB2 5, PLUS 3, SUB2 2]
16 APPL || 1 || [SUB2 6, SUB2 5, PLUS 3, SUB2 2]
17 SUB1 || 0 || [SUB2 6, SUB2 5, PLUS 3, PLUS 1]
18 APPL || 1 || [SUB2 6, SUB2 5, PLUS 3, PLUS 1]
19 APPL || 2 || [SUB2 6, SUB2 5, PLUS 3]
20 APPL || 5 || [SUB2 6, SUB2 5]
21 SUB1 || 3 || [SUB2 6, PLUS 5]
22 SUB1 || 2 || [SUB2 6, PLUS 5, SUB2 3]
23 SUB1 || 1 || [SUB2 6, PLUS 5, SUB2 3, SUB2 2]
24 APPL || 1 || [SUB2 6, PLUS 5, SUB2 3, SUB2 2]
25 SUB1 || 0 || [SUB2 6, PLUS 5, SUB2 3, PLUS 1]
26 APPL || 1 || [SUB2 6, PLUS 5, SUB2 3, PLUS 1]
27 APPL || 2 || [SUB2 6, PLUS 5, SUB2 3]
28 SUB1 || 1 || [SUB2 6, PLUS 5, PLUS 2]
29 APPL || 1 || [SUB2 6, PLUS 5, PLUS 2]
30 APPL || 3 || [SUB2 6, PLUS 5]
31 APPL || 8 || [SUB2 6]
32 SUB1 || 4 || [PLUS 8]
33 SUB1 || 3 || [PLUS 8, SUB2 4]
34 SUB1 || 2 || [PLUS 8, SUB2 4, SUB2 3]
35 SUB1 || 1 || [PLUS 8, SUB2 4, SUB2 3, SUB2 2]
36 APPL || 1 || [PLUS 8, SUB2 4, SUB2 3, SUB2 2]
37 SUB1 || 0 || [PLUS 8, SUB2 4, SUB2 3, PLUS 1]
38 APPL || 1 || [PLUS 8, SUB2 4, SUB2 3, PLUS 1]
39 APPL || 2 || [PLUS 8, SUB2 4, SUB2 3]
40 SUB1 || 1 || [PLUS 8, SUB2 4, PLUS 2]
41 APPL || 1 || [PLUS 8, SUB2 4, PLUS 2]
42 APPL || 3 || [PLUS 8, SUB2 4]
43 SUB1 || 2 || [PLUS 8, PLUS 3]
44 SUB1 || 1 || [PLUS 8, PLUS 3, SUB2 2]
45 APPL || 1 || [PLUS 8, PLUS 3, SUB2 2]
46 SUB1 || 0 || [PLUS 8, PLUS 3, PLUS 1]
47 APPL || 1 || [PLUS 8, PLUS 3, PLUS 1]
48 APPL || 2 || [PLUS 8, PLUS 3]
49 APPL || 5 || [PLUS 8]
50 APPL || 13|| []


*)
