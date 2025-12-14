type code_index = int [@@deriving show]
type stack_index = int [@@deriving show]
type heap_index = int [@@deriving show]
type offset = int [@@deriving show]
type label = string [@@deriving show]
type location = label * code_index option

let pp_location ppf : location -> unit = function
  | l, None -> Format.fprintf ppf "%s" l
  | l, Some i -> Format.fprintf ppf "%s = %d" l i

type status_code =
  | Halted
  | Running
  | Code_index_out_of_bounds
  | Stack_index_out_of_bounds
  | Heap_index_out_of_bounds
  | Stack_underflow
[@@deriving show { with_path = false }]

type stack_item =
  | STACK_INT of int
  | STACK_BOOL of bool
  | STACK_UNIT
  | STACK_HI of heap_index (* Pointer into Heap *)
  | STACK_RA of code_index (* return address *)
  | STACK_FP of stack_index (* Frame pointer *)
[@@deriving show { with_path = false }]

type heap_type = HT_PAIR | HT_INL | HT_INR | HT_CLOSURE
[@@deriving show { with_path = false }]

type heap_item =
  | HEAP_INT of int
  | HEAP_BOOL of bool
  | HEAP_UNIT
  | HEAP_HI of heap_index (* Pointer into Heap *)
  | HEAP_CI of code_index (* Code pointer for closures *)
  | HEAP_HEADER of int * heap_type (* int is number of items to follow *)
[@@deriving show { with_path = false }]

type value_path = STACK_LOCATION of offset | HEAP_LOCATION of offset
[@@deriving show { with_path = false }]

type instruction =
  | PUSH of stack_item (* modified *)
  | LOOKUP of value_path (* modified *)
  | UNARY of Ast.Unary_op.t
  | OPER of Ast.Binary_op.t
  | ASSIGN
  | SWAP
  | POP
  (*  | BIND of var            not needed *)
  | FST
  | SND
  | DEREF
  | APPLY
  | RETURN
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of location * int (* modified *)
  | TEST of location
  | CASE of location
  | GOTO of location
  | LABEL of label
  | HALT
[@@deriving show { with_path = false }]

type code = instruction list

type vm_state = {
  stack_bound : stack_index;
  code_bound : code_index;
  heap_bound : code_index;
  stack : stack_item array;
  heap : heap_item array;
  code : instruction array;
  sp : stack_index; (* stack pointer *)
  fp : stack_index; (* frame pointer *)
  cp : code_index; (* code pointer *)
  hp : heap_index; (* next free *)
  status : status_code;
}

let stack_top vm = vm.stack.(vm.sp - 1)

(********************** Printing ********************************)

let rec pp_code ppf : code -> unit = function
  | [] -> Format.fprintf ppf "\n"
  | LABEL l :: rest -> Format.fprintf ppf "%s: %a@\n" l pp_code rest
  | i :: rest -> Format.fprintf ppf "\t%a%a@\n" pp_instruction i pp_code rest

let pp_installed_code ppf (code, size) =
  let rec aux ppf k =
    if k < size then
      Format.fprintf ppf "%d: %a@\n%a" k pp_instruction code.(k) aux (k + 1)
  in
  aux ppf 0

let pp_stack ppf (sp, stack) =
  let rec aux ppf j =
    if j < sp then
      Format.fprintf ppf "%d: %a@\n%a" j pp_stack_item stack.(j) aux (j + 1)
  in
  aux ppf 0

let pp_heap ppf vm =
  let rec aux ppf k =
    if k < vm.hp then
      Format.fprintf ppf "%d -> %a\n%a" k pp_heap_item vm.heap.(k) aux (k + 1)
  in
  aux ppf 0

let pp_state ppf vm =
  Format.fprintf ppf
    "@[<v>Code Pointer:@\n\
     <1 2>%d -> %a@\n\
     Frame Pointer:@\n\
     <1 2>%d@\n\
     Stack:@\n\
     <1 2>%a@\n\
     Heap:@\n\
     <1 2>%a@]"
    vm.cp pp_instruction vm.code.(vm.cp) vm.fp pp_stack (vm.sp, vm.stack)
    pp_heap vm

(* the following two functions are needed to
   pretty-print heap and stack values
*)
let rec pp_heap_value a ppf vm =
  match vm.heap.(a) with
  | HEAP_INT i -> Format.fprintf ppf "%d" i
  | HEAP_BOOL true -> Format.fprintf ppf "true"
  | HEAP_BOOL false -> Format.fprintf ppf "false"
  | HEAP_UNIT -> Format.fprintf ppf "()"
  | HEAP_HI i -> Format.fprintf ppf "%a" (pp_heap_value i) vm
  | HEAP_CI _ ->
      Errors.complain
        "string_of_heap_value: expecting value in heap, found code index"
  | HEAP_HEADER (_, ht) -> (
      match ht with
      | HT_PAIR ->
          Format.fprintf ppf "(%a, %a)"
            (pp_heap_value (a + 1))
            vm
            (pp_heap_value (a + 2))
            vm
      | HT_INL -> Format.fprintf ppf "inl(%a)" (pp_heap_value (a + 1)) vm
      | HT_INR -> Format.fprintf ppf "inr(%a)" (pp_heap_value (a + 1)) vm
      | HT_CLOSURE -> Format.fprintf ppf "CLOSURE")

let pp_value ppf vm =
  match stack_top vm with
  | STACK_INT i -> Format.fprintf ppf "%d" i
  | STACK_BOOL true -> Format.fprintf ppf "true"
  | STACK_BOOL false -> Format.fprintf ppf "false"
  | STACK_UNIT -> Format.fprintf ppf "()"
  | STACK_HI a -> Format.fprintf ppf "%a" (pp_heap_value a) vm
  | STACK_RA _ ->
      Errors.complain "pp_value: expecting value on stack top, found code index"
  | STACK_FP _ ->
      Errors.complain
        "pp_value: expecting value on stack top, found frame pointer"

(***************************** THE MACHINE ********************************)

let readint () =
  print_string ">>> ";
  read_int ()

let stack_to_heap_item = function
  | STACK_INT i -> HEAP_INT i
  | STACK_BOOL b -> HEAP_BOOL b
  | STACK_UNIT -> HEAP_UNIT
  | STACK_HI i -> HEAP_HI i
  | STACK_RA i -> HEAP_CI i
  | STACK_FP _ ->
      Errors.complain "stack_to_heap_item: no frame pointer allowed on heap"

let heap_to_stack_item = function
  | HEAP_INT i -> STACK_INT i
  | HEAP_BOOL b -> STACK_BOOL b
  | HEAP_UNIT -> STACK_UNIT
  | HEAP_HI i -> STACK_HI i
  | HEAP_CI i -> STACK_RA i
  | HEAP_HEADER (_, _) ->
      Errors.complain "heap_to_stack_item : heap header not allowed on stack"

(* cp := cp + 1  *)
let advance_cp vm =
  if vm.cp < vm.code_bound then
    { vm with cp = vm.cp + 1 }
  else
    { vm with status = Code_index_out_of_bounds }

let goto i vm = { vm with cp = i }

(* pop n items from stack *)
let pop n vm =
  if 0 <= vm.sp - n then
    { vm with sp = vm.sp - n }
  else
    { vm with status = Stack_underflow }

let pop_top vm =
  let c = stack_top vm in
  (c, pop 1 vm)

(* pop c onto stack  *)
let push c vm =
  if vm.sp < vm.stack_bound then
    let _ = Array.set vm.stack vm.sp c in
    { vm with sp = vm.sp + 1 }
  else
    { vm with status = Stack_index_out_of_bounds }

let swap vm =
  let c1, vm = pop_top vm in
  let c2, vm = pop_top vm in
  push c2 (push c1 vm)

let do_unary : Ast.Unary_op.t * stack_item -> stack_item = function
  | Not, STACK_BOOL m -> STACK_BOOL (not m)
  | Neg, STACK_INT m -> STACK_INT (-m)
  | Read, STACK_UNIT -> STACK_INT (readint ())
  | op, _ ->
      Errors.complainf "do_unary: malformed operator: %a" Ast.Unary_op.pp op

let do_oper : Ast.Binary_op.t * stack_item * stack_item -> stack_item = function
  | And, STACK_BOOL m, STACK_BOOL n -> STACK_BOOL (m && n)
  | Or, STACK_BOOL m, STACK_BOOL n -> STACK_BOOL (m || n)
  | Eqb, STACK_BOOL m, STACK_BOOL n -> STACK_BOOL (m = n)
  | Lt, STACK_INT m, STACK_INT n -> STACK_BOOL (m < n)
  | Eqi, STACK_INT m, STACK_INT n -> STACK_BOOL (m = n)
  | Add, STACK_INT m, STACK_INT n -> STACK_INT (m + n)
  | Sub, STACK_INT m, STACK_INT n -> STACK_INT (m - n)
  | Mul, STACK_INT m, STACK_INT n -> STACK_INT (m * n)
  | Div, STACK_INT m, STACK_INT n -> STACK_INT (m / n)
  | op, _, _ ->
      Errors.complainf "do_oper: malformed operator: %a" Ast.Binary_op.pp op

let perform_op op vm =
  let v_right, vm = pop_top vm in
  let v_left, vm = pop_top vm in
  push (do_oper (op, v_left, v_right)) vm

let perform_unary op vm =
  let v, vm = pop_top vm in
  push (do_unary (op, v)) vm

(* implement garbage collection!

   This should free up all heap space
   not reachable from the stack.

   Might also increase heap size.

   Result:
   None = no progress
   Some(vm') = progress made, resulting in vm'
*)
let invoke_garbage_collection _ = None

let allocate n vm =
  let hp1 = vm.hp in
  if hp1 + n < vm.heap_bound then
    (hp1, { vm with hp = vm.hp + n })
  else
    match invoke_garbage_collection vm with
    | None -> Errors.complain "allocate : heap exhausted"
    | Some vm2 ->
        if vm2.hp + n < vm2.heap_bound then
          (vm2.hp, { vm2 with hp = vm2.hp + n })
        else
          Errors.complain "allocate : heap exhausted"

let mk_pair vm =
  let v_right, vm = pop_top vm in
  let v_left, vm = pop_top vm in
  let a, vm = allocate 3 vm in
  let header = HEAP_HEADER (3, HT_PAIR) in
  let _ = Array.set vm.heap a header in
  let _ = Array.set vm.heap (a + 1) (stack_to_heap_item v_left) in
  let _ = Array.set vm.heap (a + 2) (stack_to_heap_item v_right) in
  push (STACK_HI a) vm

let do_fst vm =
  let v, vm = pop_top vm in
  match v with
  | STACK_HI a -> (
      match vm.heap.(a) with
      | HEAP_HEADER (_, HT_PAIR) -> push (heap_to_stack_item vm.heap.(a + 1)) vm
      | _ -> Errors.complain "do_fst : unexpectd heap item")
  | _ -> Errors.complain "do_fst : expecting heap pointer on stack"

let do_snd vm =
  let v, vm = pop_top vm in
  match v with
  | STACK_HI a -> (
      match vm.heap.(a) with
      | HEAP_HEADER (_, HT_PAIR) -> push (heap_to_stack_item vm.heap.(a + 2)) vm
      | _ -> Errors.complain "do_snd : unexpectd heap item")
  | _ -> Errors.complain "do_snd : expecting heap pointer on stack"

let mk_inl vm =
  let v, vm = pop_top vm in
  let a, vm = allocate 2 vm in
  let header = HEAP_HEADER (2, HT_INL) in
  let _ = Array.set vm.heap a header in
  let _ = Array.set vm.heap (a + 1) (stack_to_heap_item v) in
  push (STACK_HI a) vm

let mk_inr vm =
  let v, vm = pop_top vm in
  let a, vm = allocate 2 vm in
  let header = HEAP_HEADER (2, HT_INR) in
  let _ = Array.set vm.heap a header in
  let _ = Array.set vm.heap (a + 1) (stack_to_heap_item v) in
  push (STACK_HI a) vm

let case i vm =
  let c, vm = pop_top vm in
  match c with
  | STACK_HI a -> (
      let vm = push (heap_to_stack_item vm.heap.(a + 1)) vm in
      match vm.heap.(a) with
      | HEAP_HEADER (_, HT_INR) -> goto i vm
      | HEAP_HEADER (_, HT_INL) -> advance_cp vm
      | _ ->
          Errors.complain "case: runtime error, expecting union header in heap")
  | _ ->
      Errors.complain
        "case: runtime error, expecting heap index on top of stack"

let mk_ref vm =
  let v, vm = pop_top vm in
  let a, vm = allocate 1 vm in
  vm.heap.(a) <- stack_to_heap_item v;
  push (STACK_HI a) vm

let deref vm =
  let v, vm = pop_top vm in
  match v with
  | STACK_HI a -> push (heap_to_stack_item vm.heap.(a)) vm
  | _ -> Errors.complain "deref"

let assign vm =
  let c1, vm = pop_top vm in
  let c2, vm = pop_top vm in
  match c2 with
  | STACK_HI a ->
      if vm.sp < vm.heap_bound then (
        vm.heap.(a) <- stack_to_heap_item c1;
        push STACK_UNIT vm)
      else
        { vm with status = Heap_index_out_of_bounds }
  | _ -> Errors.complain "assign: runtime error, expecting heap index on stack"

let test i vm =
  match stack_top vm with
  | STACK_BOOL true -> advance_cp vm
  | STACK_BOOL false -> goto i vm
  | _ -> Errors.complain "test: runtime error, expecting boolean on stack"

let return vm =
  let current_fp = vm.fp in
  match (vm.stack.(current_fp), vm.stack.(vm.fp + 1)) with
  | STACK_FP saved_fp, STACK_RA k ->
      let return_value = stack_top vm in
      push return_value { vm with cp = k; fp = saved_fp; sp = current_fp - 2 }
  | _ -> Errors.complain "return : malformed stack frame"

let fetch fp vm = function
  | STACK_LOCATION offset -> vm.stack.(fp + offset)
  | HEAP_LOCATION offset -> (
      match vm.stack.(fp - 1) with
      | STACK_HI a -> heap_to_stack_item vm.heap.(a + offset + 1)
      | _ -> Errors.complain "search : expecting closure pointer")

let lookup fp vm vlp = push (fetch fp vm vlp) vm

let mk_closure = function
  | (_, Some i), n, vm ->
      let a, vm = allocate (2 + n) vm in
      let header = HEAP_HEADER (2 + n, HT_CLOSURE) in
      let code_address = HEAP_CI i in
      vm.heap.(a) <- header;
      vm.heap.(a + 1) <- code_address;
      let rec aux m =
        if m = n then
          ()
        else
          let v = stack_to_heap_item vm.stack.(vm.sp - (m + 1)) in
          vm.heap.(a + m + 2) <- v;
          aux (m + 1)
      in
      aux 0;
      let vm = pop n vm in
      push (STACK_HI a) vm
  | (_, None), _, _ ->
      Errors.complain "mk_closure : internal error, no address in closure!"

let apply vm =
  match stack_top vm with
  | STACK_HI a -> (
      match vm.heap.(a + 1) with
      | HEAP_CI i ->
          let new_fp = vm.sp in
          let saved_fp = STACK_FP vm.fp in
          let return_index = STACK_RA (vm.cp + 1) in
          let vm = { vm with cp = i; fp = new_fp } in
          push return_index (push saved_fp vm)
      | _ ->
          Errors.complain "apply: runtime error, expecting code index in heap")
  | _ ->
      Errors.complain
        "apply: runtime error, expecting heap index on top of stack"

let step vm =
  match vm.code.(vm.cp) with
  | UNARY op -> advance_cp (perform_unary op vm)
  | OPER op -> advance_cp (perform_op op vm)
  | MK_PAIR -> advance_cp (mk_pair vm)
  | FST -> advance_cp (do_fst vm)
  | SND -> advance_cp (do_snd vm)
  | MK_INL -> advance_cp (mk_inl vm)
  | MK_INR -> advance_cp (mk_inr vm)
  | PUSH c -> advance_cp (push c vm)
  | APPLY -> apply vm
  | LOOKUP vp -> advance_cp (lookup vm.fp vm vp)
  | RETURN -> return vm
  | MK_CLOSURE (l, n) -> advance_cp (mk_closure (l, n, vm))
  | SWAP -> advance_cp (swap vm)
  | POP -> advance_cp (pop 1 vm)
  | LABEL _ -> advance_cp vm
  | DEREF -> advance_cp (deref vm)
  | MK_REF -> advance_cp (mk_ref vm)
  | ASSIGN -> advance_cp (assign vm)
  | HALT -> { vm with status = Halted }
  | GOTO (_, Some i) -> goto i vm
  | TEST (_, Some i) -> test i vm
  | CASE (_, Some i) -> case i vm
  | _ -> Errors.complainf "step : bad state = %a" pp_state vm

(* DRIVER *)

let rec driver n vm =
  if Option.verbose then
    Format.printf "========== state %d ==========@.%a@." n pp_state vm;

  if vm.status = Running then
    driver (n + 1) (step vm)
  else
    vm

let map_instruction_labels f = function
  | GOTO (lab, _) -> GOTO (lab, Some (f lab))
  | TEST (lab, _) -> TEST (lab, Some (f lab))
  | CASE (lab, _) -> CASE (lab, Some (f lab))
  | MK_CLOSURE ((lab, _), n) -> MK_CLOSURE ((lab, Some (f lab)), n)
  | inst -> inst

let find y vmap =
  match List.assoc_opt y vmap with
  | None -> Errors.complainf "Compile.find : %s is not found" y
  | Some v -> v

(* put code listing into an array, associate an array index to each label *)
let load (instr_list : code) : instruction array * int =
  (* find array index for each label *)
  let mk_label_to_address l =
    let rec aux carry k = function
      | [] -> carry
      | LABEL lab :: rest -> aux ((lab, k) :: carry) (k + 1) rest
      | _ :: rest -> aux carry (k + 1) rest
    in
    aux [] 0 l
  in
  let label_to_address = mk_label_to_address instr_list in
  let locate_instr =
    map_instruction_labels (fun x -> find x label_to_address)
  in
  let located_instr_list = List.map locate_instr instr_list in
  let result = Array.of_list located_instr_list in
  (result, Array.length result)

let initial_state l =
  let code_array, c_bound = load l in

  if Option.verbose then
    Format.printf "Installed Code = \n%a" pp_installed_code (code_array, c_bound);

  {
    stack_bound = Option.stack_max;
    heap_bound = Option.heap_max;
    code_bound = c_bound;
    stack = Array.make Option.stack_max (STACK_INT 0);
    heap = Array.make Option.heap_max (HEAP_INT 0);
    code = code_array;
    sp = 0;
    fp = 0;
    cp = 0;
    hp = 0;
    status = Running;
  }

let first_frame vm =
  let saved_fp = STACK_FP 0 in
  let return_index = STACK_RA 0 in
  push return_index (push saved_fp vm)

let run l =
  let vm = driver 1 (first_frame (initial_state l)) in

  match vm.status with
  | Halted -> vm
  | status ->
      Errors.complainf "run : stopped wth status %a" pp_status_code status

(* COMPILE *)

let label_ref = ref 0

let new_label () =
  let v = !label_ref in
  label_ref := !label_ref + 1;
  Format.sprintf "L%d" v

(*

Interp 2

 | (APPLY :: ds,  V(CLOSURE (_, (c, env))) :: (V v) :: evs)
    -> (c @ ds, (V v) :: (EV env) :: evs)

Interp 3

 | (APPLY,  V(CLOSURE ((_, Some i), env)) :: (V v) :: evs)
    -> (i, (V v) :: (EV env) :: (RA (cp + 1)) :: evs)


Jargon VM :

     [closure    ]
     [arg        ]
        ...

 == APPLY ==>

     [return address]
fp ->[old fp        ]
     [clsoure       ]
     [arg           ]
        ...

*)

let positions l =
  let rec aux k = function
    | [] -> []
    | a :: rest -> (a, k) :: aux (k + 1) rest
  in
  aux 1 l

let rec comp vmap : Ast.t -> code * code = function
  | Unit -> ([], [ PUSH STACK_UNIT ])
  | Boolean b -> ([], [ PUSH (STACK_BOOL b) ])
  | Integer n -> ([], [ PUSH (STACK_INT n) ])
  | UnaryOp (op, e) ->
      let defs, c = comp vmap e in
      (defs, c @ [ UNARY op ])
  | BinaryOp (e1, op, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [ OPER op ])
  | Pair (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [ MK_PAIR ])
  | Fst e ->
      let defs, c = comp vmap e in
      (defs, c @ [ FST ])
  | Snd e ->
      let defs, c = comp vmap e in
      (defs, c @ [ SND ])
  | Inl e ->
      let defs, c = comp vmap e in
      (defs, c @ [ MK_INL ])
  | Inr e ->
      let defs, c = comp vmap e in
      (defs, c @ [ MK_INR ])
  | Case (e1, (x1, e2), (x2, e3)) ->
      let inr_label = new_label () in
      let after_inr_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap (Lambda (x1, e2)) in
      let defs3, c3 = comp vmap (Lambda (x2, e3)) in
      ( defs1 @ defs2 @ defs3,
        c1
        @ [ CASE (inr_label, None) ]
        @ c2
        @ [ APPLY; GOTO (after_inr_label, None); LABEL inr_label ]
        @ c3
        @ [ APPLY; LABEL after_inr_label ] )
  | If (e1, e2, e3) ->
      let else_label = new_label () in
      let after_else_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      let defs3, c3 = comp vmap e3 in
      ( defs1 @ defs2 @ defs3,
        c1
        @ [ TEST (else_label, None) ]
        @ c2
        @ [ GOTO (after_else_label, None); LABEL else_label ]
        @ c3 @ [ LABEL after_else_label ] )
  | Seq [] -> ([], [])
  | Seq [ e ] -> comp vmap e
  | Seq (e :: rest) ->
      let defs1, c1 = comp vmap e in
      let defs2, c2 = comp vmap (Seq rest) in
      (defs1 @ defs2, c1 @ [ POP ] @ c2)
  | Ref e ->
      let defs, c = comp vmap e in
      (defs, c @ [ MK_REF ])
  | Deref e ->
      let defs, c = comp vmap e in
      (defs, c @ [ DEREF ])
  | While (e1, e2) ->
      let test_label = new_label () in
      let end_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      ( defs1 @ defs2,
        [ LABEL test_label ] @ c1
        @ [ TEST (end_label, None) ]
        @ c2
        @ [ POP; GOTO (test_label, None); LABEL end_label; PUSH STACK_UNIT ] )
  | Assign (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [ ASSIGN ])
  | App (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c2 @ c1 @ [ APPLY ])
  | Var x -> ([], [ LOOKUP (find x vmap) ])
  | LetFun (f, (x, e1), e2) -> comp vmap (App (Lambda (f, e2), Lambda (x, e1)))
  | Lambda (x, e) -> comp_lambda vmap (None, x, e)
  | LetRecFun (f, (x, e1), e2) ->
      let defs1, c1 = comp vmap (Lambda (f, e2)) in
      let defs2, c2 = comp_lambda vmap (Some f, x, e1) in
      (defs1 @ defs2, c2 @ c1 @ [ APPLY ])

and comp_lambda vmap (f_opt, x, e) =
  let bound_vars = match f_opt with None -> [ x ] | Some f -> [ x; f ] in
  let f = new_label () in
  let f_bind =
    match f_opt with None -> [] | Some f -> [ (f, STACK_LOCATION (-1)) ]
  in
  let x_bind = (x, STACK_LOCATION (-2)) in
  let fvars = Free_vars.free_vars bound_vars e in
  let fetch_fvars = List.map (fun y -> LOOKUP (find y vmap)) fvars in
  let fvar_bind (y, p) = (y, HEAP_LOCATION p) in
  let env_bind = List.map fvar_bind (positions fvars) in
  let new_vmap = x_bind :: (f_bind @ env_bind @ vmap) in
  let defs, c = comp new_vmap e in
  let def = [ LABEL f ] @ c @ [ RETURN ] in
  ( def @ defs,
    List.rev fetch_fvars @ [ MK_CLOSURE ((f, None), List.length fvars) ] )

let compile e =
  let defs, c = comp [] e in
  (* body of program @ stop the interpreter @ function definitions *)
  let result = c @ [ HALT ] @ defs in

  if Option.verbose then
    Format.printf "Compiled Code = \n%a" pp_code result;

  result

let interpret e = run (compile e)
let reset () = label_ref := 0
