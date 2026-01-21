type 'f t =
  | Ref of Heap.address
  | Int of int [@printer Format.pp_print_int]
  | Bool of bool
  | Unit
  | Pair of 'f t * 'f t
  | Inl of 'f t
  | Inr of 'f t
  | Fun of 'f
[@@deriving show { with_path = false }]

open Format

let rec pp pp_fun ppf = function
  | Ref a -> fprintf ppf "Ref(%a)" Heap.pp_address a
  | Bool b -> fprintf ppf "%b" b
  | Int n -> fprintf ppf "%d" n
  | Unit -> fprintf ppf "()"
  | Pair (v1, v2) -> fprintf ppf "(@[%a,@ %a)@]" (pp pp_fun) v1 (pp pp_fun) v2
  | Inl v -> fprintf ppf "Inl(%a)" (pp pp_fun) v
  | Inr v -> fprintf ppf "Inr(%a)" (pp pp_fun) v
  | Fun f -> pp_fun ppf f
