exception Error of string

let complain s = raise (Error s)
let complainf fmt = Format.kasprintf complain fmt
