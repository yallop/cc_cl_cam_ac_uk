
type code_index = int 
type stack_index = int 
type heap_index = int 
type static_distance = int 
type offset  = int 

type label = string 
type location = label * (code_index option) 

type status_code = 
  | Halted 
  | Running 
  | CodeIndexOutOfBound 
  | StackIndexOutOfBound 
  | HeapIndexOutOfBound
  | StackUnderflow 

type stack_item = 
  | STACK_INT of int 
  | STACK_BOOL of bool 
  | STACK_UNIT 
  | STACK_HI of heap_index    (* Pointer into Heap            *) 
  | STACK_RA of code_index    (* return address               *) 
  | STACK_FP of stack_index   (* Frame pointer                *) 

type heap_type = 
    | HT_PAIR 
    | HT_INL 
    | HT_INR 
    | HT_CLOSURE 

type heap_item = 
  | HEAP_INT of int 
  | HEAP_BOOL of bool 
  | HEAP_UNIT 
  | HEAP_HI of heap_index    (* Pointer into Heap            *) 
  | HEAP_CI of code_index    (* Code pointer for closures    *) 
  | HEAP_HEADER of int * heap_type (* int is number of items to follow *)       

type value_path = 
  | STACK_LOCATION of offset 
  | HEAP_LOCATION of offset 

type 'a instruction =
  | PUSH of 'a * stack_item    (* modified *)
  | LOOKUP of 'a * value_path      (* modified *)
  | UNARY of 'a * Ast.unary_oper
  | OPER of 'a * Ast.oper
  | ASSIGN of 'a
  | SWAP of 'a
  | POP of 'a
(*  | BIND of var            not needed *)
  | FST of 'a
  | SND of 'a
  | DEREF of 'a
  | APPLY of 'a
  | RETURN of 'a
  | MK_PAIR of 'a
  | MK_INL of 'a
  | MK_INR of 'a
  | MK_REF of 'a
  | MK_CLOSURE of 'a * location * int (* modified *)
  | TEST of 'a * location
  | CASE of 'a * location
  | GOTO of 'a * location
  | LABEL of 'a * label
  | HALT of 'a

type vm_state = 
  {
    stack_bound : stack_index; 
    code_bound  : code_index; 
    heap_bound  : code_index; 
    stack       : stack_item array; 
    heap        : heap_item array; 
    code        : unit instruction array;
    mutable sp : stack_index;  (* stack pointer *) 
    mutable fp : stack_index;  (* frame pointer *) 
    mutable cp : code_index;   (* code pointer  *) 
    mutable hp : heap_index;   (* next free     *) 
    mutable status : status_code; 
  } 

val new_label : unit -> string
			  
val step : vm_state -> vm_state 

val driver : int -> vm_state -> vm_state 

type 'a listing = 'a instruction list

val comp : (Past.var * value_path) list ->
           'a Ast.expr -> 'a instruction list * 'a instruction list

val compile : 'a Ast.expr -> 'a listing

val run : unit listing -> vm_state

val interpret : 'a Ast.expr -> vm_state

val string_of_listing : 'a listing -> string

val string_of_stack_item : stack_item -> string 

val string_of_status : status_code -> string 

val string_of_heap_item : heap_item -> string 

val string_of_heap_type : heap_type -> string 

val string_of_instruction : 'a instruction -> string

val string_of_value : vm_state -> string 

val string_of_location : location -> string

val string_of_value_path: value_path -> string

val reset : unit -> unit

val first_frame : vm_state -> vm_state

val initial_state : unit instruction list -> vm_state

val map : ('a -> 'b) -> 'a instruction -> 'b instruction
