module Int_map = Map.Make (Int)

type address = int
type 'a t = 'a Int_map.t

let empty : 'a t = Int_map.empty
let size : 'a t -> int = Int_map.cardinal

let alloc (v : 'a) (h : 'a t) : address * 'a t =
  let next = size h in
  if next < Option.heap_max then
    (next, Int_map.add next v h)
  else
    Errors.complain "Runtime error: out of heap space"

let get (a : address) (h : 'a t) : 'a =
  match Int_map.find_opt a h with
  | None -> Errors.complain "Runtime error: invalid heap address"
  | Some v -> v

let set (a : address) (v : 'a) (h : 'a t) : 'a t =
  Int_map.update a
    (function
      | None -> Errors.complain "Runtime error: invalid heap address"
      | Some _ -> Some v)
    h

let to_list : 'a t -> (address * 'a) list = Int_map.bindings
let pp_binding pp_value ppf (a, v) = Format.fprintf ppf "%d -> %a" a pp_value v

let pp pp_value ppf (h : 'a t) =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "@;")
    (pp_binding pp_value) ppf (Int_map.bindings h)

let pp_address = Format.pp_print_int
