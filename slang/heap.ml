(* Heap implementation shared by interpreters 0, 1, 2, 3 (Jargon VM uses an array) *)

module Int_map = Map.Make (Int)

type address = int
(** Address is an alias for int, but this is hidden in the interface *)

type 'a t = { bindings : 'a Int_map.t; next : int }
(** A heap is a map from integers to values, together with the next available
    integer. *)

let empty : 'a t = { bindings = Int_map.empty; next = 0 }

let alloc (v : 'a) (h : 'a t) : address * 'a t =
  if h.next < Option.heap_max then
    (h.next, { bindings = Int_map.add h.next v h.bindings; next = h.next + 1 })
  else
    Errors.complain "Runtime error: out of heap space"

let get (a : address) (h : 'a t) : 'a =
  match Int_map.find_opt a h.bindings with
  | None -> Errors.complain "Runtime error: invalid heap address"
  | Some v -> v

let set (a : address) (v : 'a) (h : 'a t) : 'a t =
  {
    h with
    bindings =
      Int_map.update a
        (function
          | None -> Errors.complain "Runtime error: invalid heap address"
          | Some _ -> Some v)
        h.bindings;
  }

let to_list (h : 'a t) : (address * 'a) list = Int_map.bindings h.bindings
let size (h : 'a t) : int = h.next
let pp_binding pp_value ppf (a, v) = Format.fprintf ppf "%d -> %a" a pp_value v

let pp pp_value ppf (h : 'a t) =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "@;")
    (pp_binding pp_value) ppf
    (Int_map.bindings h.bindings)

let pp_address = Format.pp_print_int
