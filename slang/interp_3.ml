(**************************************
Compiler Construction 2020
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 
(* 
   Interpreter 3. 

   Derived from Interpreter 2 by 
   --- Make instructions linear by introducing 
       labels and jumps.  
   --- labels translated to numeric addresses. 
   --- include "code pointer" in state 
   --- compiler elimnates WHILE construct
*) 


open Ast 

let complain = Errors.complain

type address = int 

type label = string 

type location = label * (address option) 

type value = 
     | REF of address 
     | INT of int 
     | BOOL of bool 
     | UNIT
     | PAIR of value * value 
     | INL of value 
     | INR of value 
     | CLOSURE of location * env
     | REC_CLOSURE of location

and 'a instruction =
  | PUSH of 'a * value
  | LOOKUP of 'a * Ast.var
  | UNARY of 'a * Ast.unary_oper
  | OPER of 'a * Ast.oper
  | ASSIGN of 'a
  | SWAP of 'a
  | POP of 'a
  | BIND of 'a * Ast.var
  | FST of 'a
  | SND of 'a
  | DEREF of 'a
  | APPLY of 'a
  | RETURN of 'a
  | MK_PAIR of 'a
  | MK_INL of 'a
  | MK_INR of 'a
  | MK_REF of 'a
  | MK_CLOSURE of 'a * location
  | MK_REC of 'a * Ast.var * location
  | TEST of 'a * location
  | CASE of 'a * location
  | GOTO of 'a * location
  | LABEL of 'a * label
  | HALT of 'a

and 'a code = 'a instruction list

and binding = var * value

and env = binding list

let map f = function
 | UNARY(a, op) -> UNARY(f a, op)
 | OPER(a, op)  -> OPER(f a, op)
 | MK_PAIR a  -> MK_PAIR(f a)
 | FST a    -> FST(f a)
 | SND a    -> SND(f a)
 | MK_INL a -> MK_INL(f a)
 | MK_INR a -> MK_INR(f a)
 | MK_REF a -> MK_REF(f a)
 | PUSH(a, op)   -> PUSH(f a, op)
 | LOOKUP(a, x) -> LOOKUP(f a, x)
 | TEST(a, label)   -> TEST(f a, label)  
 | CASE(a, label)   -> CASE(f a, label)  
 | GOTO(a, label)   -> GOTO(f a, label)  
 | APPLY a  -> APPLY(f a)
 | RETURN a -> RETURN (f a)
 | HALT a   -> HALT (f a)
 | BIND(a, x)   -> BIND(f a, x)  
 | LABEL(a, label)  -> LABEL(f a, label) 
 | SWAP a   -> SWAP (f a)
 | POP a    -> POP (f a)
 | DEREF a  -> DEREF (f a)
 | ASSIGN a -> ASSIGN (f a)
 | MK_CLOSURE(a, loc)  -> MK_CLOSURE(f a, loc) 
 | MK_REC(a, v, loc) -> MK_REC(f a, v, loc)

type env_or_value = 
  | EV of env        (* an environment on the run-time stack *)
  | V of value       (* a value on the run-time stack *)
  | RA of address    (* a return address on the run-time stack *)

type env_value_stack = env_or_value list 

type state = address * env_value_stack 

(* update : (env * binding) -> env *) 
(* let update(env, (x, v)) = (x, v) :: env *)

let rec lookup (env, x) = 
    match env with 
    | [] -> None
    | (y, v) :: rest -> if x = y then Some(match v with
        | REC_CLOSURE(loc) -> CLOSURE(loc, (y, REC_CLOSURE loc)::rest)
        | _ -> v)
      else lookup (rest, x)

let rec search (evs, x) = 
  match evs with 
  | [] -> complain (x ^ " is not defined!\n")
  | (V _) :: rest -> search (rest, x) 
  | (RA _) :: rest -> search (rest, x) 
  | (EV env) :: rest -> 
    (match lookup(env, x) with 
    | None -> search (rest, x) 
    | Some v -> v 
    ) 
 let rec evs_to_env = function 
  | [] -> []
  | (V _) :: rest -> evs_to_env rest 
  | (RA _) :: rest -> evs_to_env rest 
  | (EV env) :: rest -> env @ (evs_to_env rest) 

let string_of_list sep f l = 
   let rec aux f = function 
     | [] -> ""
     | [t] -> (f t)
     | t :: rest -> (f t) ^  sep  ^ (aux f rest)
   in "[" ^ (aux f l) ^ "]"
    
let rec string_of_value = function 
     | REF a          -> "REF(" ^ (string_of_int a) ^ ")"
     | BOOL b         -> string_of_bool b
     | INT n          -> string_of_int n 
     | UNIT           -> "UNIT"
     | PAIR(v1, v2)    -> "(" ^ (string_of_value v1) ^ ", " ^ (string_of_value v2) ^ ")"
     | INL v           -> "inl(" ^ (string_of_value v) ^ ")"
     | INR  v          -> "inr(" ^ (string_of_value v) ^ ")"
     | CLOSURE (loc, c) -> "CLOSURE(" ^ (string_of_closure (loc, c)) ^ ")"
     | REC_CLOSURE(loc) -> "REC_CLOSURE(" ^ (string_of_location loc) ^ ")"

and string_of_closure (loc, env) = 
   "(" ^ (string_of_location loc) ^ ", " ^ (string_of_env env) ^ ")"

and string_of_env env = string_of_list ",\n " string_of_binding env 

and string_of_binding (x, v) =    "(" ^ x ^ ", " ^ (string_of_value v) ^ ")"

and string_of_location = function 
  | (l, None) -> l 
  | (l, Some i) -> l ^ " = " ^ (string_of_int i) 

and string_of_instruction = function
 | UNARY(_, op) -> "UNARY " ^ (string_of_uop op)
 | OPER(_, op)  -> "OPER " ^ (string_of_bop op)
 | MK_PAIR _  -> "MK_PAIR"
 | FST _    -> "FST"
 | SND _    -> "SND"
 | MK_INL _ -> "MK_INL"
 | MK_INR _ -> "MK_INR"
 | MK_REF _ -> "MK_REF"
 | PUSH(_, v)   -> "PUSH " ^ (string_of_value v)
 | LOOKUP(_, x) -> "LOOKUP " ^ x
 | TEST(_, label)   -> "TEST " ^ (string_of_location label)
 | CASE(_, label)   -> "CASE " ^ (string_of_location label)
 | GOTO(_, label)   -> "GOTO " ^ (string_of_location label)
 | APPLY _  -> "APPLY"
 | RETURN _ -> "RETURN"
 | HALT _   -> "HALT"
 | BIND(_, x)   -> "BIND " ^ x
 | LABEL(_, label)  -> "LABEL " ^ label
 | SWAP _   -> "SWAP"
 | POP _    -> "POP"
 | DEREF _  -> "DEREF"
 | ASSIGN _ -> "ASSIGN"
 | MK_CLOSURE(_, loc)  -> "MK_CLOSURE(" ^ (string_of_location loc) ^ ")"
 | MK_REC(_, v, loc) -> "MK_REC(" ^ v ^ ", " ^ (string_of_location loc) ^ ")"

and string_of_code c = string_of_list "\n " string_of_instruction c 

let string_of_env_or_value = function 
  | EV env -> "EV " ^ (string_of_env env)
  | V v -> "V " ^ (string_of_value v)
  | RA i -> "RA " ^ (string_of_int i)

let string_of_env_value_stack = string_of_list ";\n " string_of_env_or_value 

(* THE MACHINE *) 

let installed = ref (Array.of_list [HALT ()])

let string_of_installed_code ()  = 
    let size = Array.length !installed in 
    let rec aux k = 
            if size = k 
	    then "" 
	    else (string_of_int k) ^ ": " 
                  ^ (string_of_instruction (!installed.(k))) 
                  ^ "\n" ^ (aux (k+1)) 
    in aux 0

let get_instruction cp = Array.get !installed cp

let heap  = Array.make Option.heap_max (INT 0)

let next_address = ref 0 

let new_address () = let a = !next_address in (next_address := a + 1; a) 

let string_of_heap ()  = 
    let rec aux k = 
            if !next_address < k 
	    then "" 
	    else (string_of_int k) ^ " -> " ^ (string_of_value (heap.(k))) ^ "\n" ^ (aux (k+1)) 
    in "\nHeap = \n" ^ (aux 0) 
    
let string_of_state (cp, evs) = 
    "\nCode Pointer = " 
    ^ (string_of_int cp) ^ " -> " 
    ^ (string_of_instruction  (get_instruction cp)) 
    ^ "\nStack = \n" 
    ^ (string_of_env_value_stack evs) 
    ^ (if !next_address = 0 then "" else string_of_heap()) 

let readint () = let _ = print_string "input> " in read_int() 

let do_unary = function 
  | (NOT,  BOOL m) -> BOOL (not m)
  | (NEG,  INT m)  -> INT (-m)
  | (READ, UNIT)   -> INT (readint())
  | (op, _) -> complain ("malformed unary operator: " ^ (string_of_unary_oper op))

let do_oper = function 
  | (AND,  BOOL m,  BOOL n) -> BOOL (m && n)
  | (OR,   BOOL m,  BOOL n) -> BOOL (m || n)
  | (EQB,  BOOL m,  BOOL n) -> BOOL (m = n)
  | (LT,   INT m,   INT n)  -> BOOL (m < n)
  | (EQI,  INT m,   INT n)  -> BOOL (m = n)
  | (ADD,  INT m,   INT n)  -> INT (m + n)
  | (SUB,  INT m,   INT n)  -> INT (m - n)
  | (MUL,  INT m,   INT n)  -> INT (m * n)
  | (DIV,  INT m,   INT n)  -> INT (m / n)
  | (op, _, _)  -> complain ("malformed binary operator: " ^ (string_of_oper op))


let step (cp, evs) =
 match (get_instruction cp, evs) with
 | (PUSH (_, v),                            evs) -> (cp + 1, (V v) :: evs)
 | (POP _,                          _ :: evs) -> (cp + 1, evs)
 | (SWAP _,                  s1 :: s2 :: evs) -> (cp + 1, s2 :: s1 :: evs)
 | (BIND(_, x),                   (V v) :: evs) -> (cp + 1, EV([(x, v)]) :: evs)
 | (LOOKUP (_, x),                          evs) -> (cp + 1, V(search(evs, x)) :: evs)
 | (UNARY (_, op),                 (V v) :: evs) -> (cp + 1, V(do_unary(op, v)) :: evs)
 | (OPER (_, op),       (V v2) :: (V v1) :: evs) -> (cp + 1, V(do_oper(op, v1, v2)) :: evs)
 | (MK_PAIR _,       (V v2) :: (V v1) :: evs) -> (cp + 1, V(PAIR(v1, v2)) :: evs)
 | (FST _,             V(PAIR (v, _)) :: evs) -> (cp + 1, (V v) :: evs)
 | (SND _,             V(PAIR (_, v)) :: evs) -> (cp + 1, (V v) :: evs)
 | (MK_INL _,                   (V v) :: evs) -> (cp + 1, V(INL v) :: evs)
 | (MK_INR _,                   (V v) :: evs) -> (cp + 1, V(INR v) :: evs)
 | (CASE (_, (_, Some _)),        V(INL v)::evs) -> (cp + 1, (V v) :: evs)
 | (CASE (_, (_, Some i)),        V(INR v)::evs) -> (i,      (V v) :: evs)
 | (TEST (_, (_, Some _)),  V(BOOL true) :: evs) -> (cp + 1, evs)
 | (TEST (_, (_, Some i)), V(BOOL false) :: evs) -> (i,      evs)
 | (ASSIGN _,    (V v) :: (V (REF a)) :: evs) -> (heap.(a) <- v; (cp + 1, V(UNIT) :: evs))
 | (DEREF _,              (V (REF a)) :: evs) -> (cp + 1, V(heap.(a)) :: evs)
 | (MK_REF _,                   (V v) :: evs) -> let a = new_address () in (heap.(a) <- v;
                                               (cp + 1, V(REF a) :: evs))
 | (MK_CLOSURE (_, loc),                    evs) -> (cp + 1, V(CLOSURE(loc, evs_to_env evs)) :: evs)
 | (MK_REC (_, f, loc),                   evs) -> (cp + 1, V(CLOSURE(loc, (f, REC_CLOSURE loc):: evs_to_env evs)) :: evs)
 | (APPLY _,  V(CLOSURE ((_, Some i), env)) :: (V v) :: evs)
                                            -> (i, (V v) :: (EV env) :: (RA (cp + 1)) :: evs)
(* new intructions *)
 | (RETURN _,    (V v) :: _ :: (RA i) :: evs) -> (i, (V v) :: evs)
 | (LABEL (_, _),                           evs) -> (cp + 1, evs)
 | (HALT _,                              evs) -> (cp, evs)
 | (GOTO (_, (_, Some i)),                  evs) -> (i, evs)
 | _ -> complain ("step : bad state = " ^ (string_of_state (cp, evs)) ^ "\n")

(* COMPILE *) 

let label_ref = ref 0
let new_label = 
    let get () = let v = !label_ref in (label_ref := (!label_ref) + 1; "L"^ (string_of_int v))
    in get 

let rec comp = function
  | Unit l           -> ([], [PUSH (l, UNIT)])
  | Integer(l, n)      -> ([], [PUSH (l, (INT n))])
  | Boolean(l, b)      -> ([], [PUSH (l, (BOOL b))])
  | Var(l, x)          -> ([], [LOOKUP (l, x)])
  | UnaryOp(l, op, e) -> let (defs, c) = comp e in  (defs, c @ [UNARY (l, op)])
  | Op(l, e1, op, e2) -> let (defs1, c1) = comp e1 in
                      let (defs2, c2) = comp e2 in
                          (defs1 @ defs2, c1 @ c2 @ [OPER(l, op)])
  | Pair(l, e1, e2)   -> let (defs1, c1) = comp e1 in
                      let (defs2, c2) = comp e2 in
                          (defs1 @ defs2, c1 @ c2 @ [MK_PAIR l])
  | Fst(l, e)          -> let (defs, c) = comp e in (defs, c @ [FST l])
  | Snd(l, e)          -> let (defs, c) = comp e in (defs, c @ [SND l])
  | Inl(l, e)          -> let (defs, c) = comp e in (defs, c @ [MK_INL l])
  | Inr(l, e)          -> let (defs, c) = comp e in (defs, c @ [MK_INR l])
  | Case(l, e1, (l', x1, e2), (l'', x2, e3)) ->
                      let inr_label = new_label () in
                      let after_inr_label = new_label () in
                      let (defs1, c1) = comp e1 in
                      let (defs2, c2) = comp e2 in
                      let (defs3, c3) = comp e3 in
                         (defs1 @ defs2 @ defs3,
                          (c1
   		           @ [CASE(l, (inr_label, None))]
                           @ ((BIND (l', x1)) :: c2 @ [SWAP l; POP l])
		           @ [GOTO (l'', ((after_inr_label, None))); LABEL (l',inr_label)]
                           @ ((BIND (l'', x2)) :: c3 @ [SWAP l; POP l])
		           @ [LABEL(l'', after_inr_label)]))
  | If(l, e1, e2, e3) -> let else_label = new_label () in
                      let after_else_label = new_label () in
                      let (defs1, c1) = comp e1 in
                      let (defs2, c2) = comp e2 in
                      let (defs3, c3) = comp e3 in
                         (defs1 @ defs2 @ defs3,
                          (c1
   		           @ [TEST(l, (else_label, None))]
                           @ c2
		           @ [GOTO (l, (after_else_label, None)); LABEL(l, else_label)]
                           @ c3
		           @ [LABEL (l, after_else_label)]))
 | Seq(_, [])         -> ([], [])
 | Seq(_, [e])        -> comp e
 | Seq(l, (e ::rest)) -> let (defs1, c1) = comp e in
                     let (defs2, c2) = comp (Seq (l, rest)) in
                       (defs1 @ defs2, c1 @ [POP l] @ c2)
 | Ref(l, e)          -> let (defs, c) = comp e in (defs, c @ [MK_REF l])
 | Deref(l, e)        -> let (defs, c) = comp e in (defs, c @ [DEREF l])
 | While(l, e1, e2)  -> let test_label = new_label () in
                     let end_label = new_label () in
                     let (defs1, c1) = comp e1 in
                     let (defs2, c2) = comp e2 in
                         (defs1 @ defs2,
                          [LABEL (l, test_label)]
                           @ c1
                           @ [TEST(l, (end_label, None))]
                           @ c2
                           @ [POP l; GOTO (l, (test_label, None)); LABEL (l, end_label); PUSH (l, UNIT)])
 | Assign(l, e1, e2) -> let (defs1, c1) = comp e1 in
                     let (defs2, c2) = comp e2 in
                         (defs1 @ defs2, c1 @ c2 @ [ASSIGN l])
 | App(l, e1, e2)    -> let (defs1, c1) = comp e1 in
                      let (defs2, c2) = comp e2 in
                          (defs1 @ defs2, c2 @ c1 @ [APPLY l])
 | Lambda(l, x, e)    -> let (defs, c) = comp e in
                      let f = new_label () in
                      let def = [LABEL (l, f) ; BIND(l, x)] @ c @ [SWAP l; POP l; RETURN l] in
                          (def @ defs, [MK_CLOSURE(l, (f, None))])
(*
 Note that we could have

 | LetFun(f, (x, e1), e2) -> comp (App(Lambda(f, e2), Lambda(x, e1)))

 This would then result (ignoring the defs generated by subterms) in 

    defs = [LABEL g ; BIND f] @ c2 @ [SWAP; POP; RETURN]
         @ [LABEL h; bind x ] @ c1 @ [SWAP; POP; RETURN]

    code = [MK_CLOSURE((h, None)); [MK_CLOSURE((g, None)); APPLY] 

  where g and h are new labels. 

  In contrast, the following version of comp results in 

     defs = [LABEL f; BIND x] @ c1 @ [SWAP; POP; RETURN] 

     code = [MK_CLOSURE((f, None)); BIND f] @ c2 @ [SWAP; POP])

  which is simpler. 

*)
 | LetFun(l, f, (l', x, e1), e2) ->
                      let (defs1, c1) = comp e1 in
                      let (defs2, c2) = comp e2 in
                      let lab = new_label () in
                      let def = [LABEL(l, lab); BIND(l',x)] @ c1 @ [SWAP l; POP l; RETURN l] in
                          (def @ defs1 @ defs2,
                           [MK_CLOSURE(l, (lab, None)); BIND (l, f)] @ c2 @ [SWAP l; POP l])
 | LetRecFun(l, f, (l', x, e1), e2) ->
                      let (defs1, c1) = comp e1 in
                      let (defs2, c2) = comp e2 in
                      let lab = new_label () in
                      let def = [LABEL(l, lab); BIND(l', x)] @ c1 @ [SWAP l; POP l; RETURN l] in
                          (def @ defs1 @ defs2,
                           [MK_REC(l, f, (lab, None)); BIND(l, f)] @ c2 @ [SWAP l; POP l])
let compile e =
    let (defs, c) = comp e in
    (* The HALT instruction needs an annotation to satisfy the types
       We arbitarily use the annotation from the root of the AST 
    *)
    let l = get_tag e in
    let result = c @               (* body of program *)
                   [HALT l]          (* stop the interpreter *)
                   @ defs in       (* the function definitions *)
    let _ = if Option.verbose
            then print_string ("\nCompiled Code = \n" ^ (string_of_code result))
            else () 
    in result 

let rec driver n state = 
  let _ = if Option.verbose 
          then print_string ("\nstate " ^ (string_of_int n) ^ ":" ^ (string_of_state state) ^ "\n")
          else ()
  in match state with
     | (cp, evs) ->
       if (HALT ()) = map (fun _ -> ()) @@ get_instruction cp
       then (match evs with
             | [V v] -> v
             | _ -> complain ("driver : bad halted state = " ^ (string_of_state state) ^ "\n"))
       else driver (n + 1) (step state) 

(* put code listing into an array, associate an array index to each label *) 
let load l = 
let rec find lab = function 
     | [] -> complain ("find : " ^ lab ^ " is not found")
     | (x, v) :: rest -> if x = lab then v else find lab rest 
    (* insert array index for each label *) 
   in let apply_label_map_to_instruction m = function 
     | GOTO (l, (lab, _)) -> GOTO(l, (lab, Some(find lab m)))
     | TEST (l, (lab, _)) -> TEST(l, (lab, Some(find lab m)))
     | CASE (l, (lab, _)) -> CASE(l, (lab, Some(find lab m)))
     | MK_CLOSURE (l, (lab, _)) -> MK_CLOSURE(l, (lab, Some(find lab m)))
     | MK_REC (l, f, (lab, _)) -> MK_REC(l, f, (lab, Some(find lab m)))
(* 
     | MK_CLOSURE ((lab, _), fvars) -> MK_CLOSURE((lab, Some(find lab m)), fvars)
*) 
     | inst -> inst 
   (* find array index for each label *) 
   in let listing_to_label_map l = 
       let rec aux carry k = function 
         | [] -> carry 
         | (LABEL (_, lab)) :: rest -> aux ((lab, k) :: carry) (k +1) rest
         | _ :: rest           -> aux carry (k+1) rest 
       in aux [] 0 l 
    in let l_map = listing_to_label_map l 
    in Array.of_list (List.map (apply_label_map_to_instruction l_map) l)


(* interpret : expr -> value *) 
let interpret e = 
    (* Remove annotation from AST *)
    let e' = Ast.map (fun _ -> ()) e in
    let c = compile e' in
    let _ = installed := load c in 
    let _ = if Option.verbose 
            then print_string ("\nInstalled Code = \n" ^ (string_of_installed_code()))
            else () 
    (* set the code pointer to 0 *) 
    in driver 1 (0 , [])

let reset = fun _ -> next_address := 0; label_ref := 0; Array.fill heap 0 (Array.length heap) (INT 0)

