exception Error of string

let complain s = raise (Error s)
let complainf ppf = Format.kasprintf complain ppf
