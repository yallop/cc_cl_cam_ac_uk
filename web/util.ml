open Js_of_ocaml

(* Don't be afraid of this ugly type. It creates a stream like object in js.*)
type stream = <
   steps : Js.js_string Js.t Js.readonly_prop;
   next : stream Js.meth
> Js_of_ocaml.Js.t

let rec errStream s : stream = (object%js
  val steps = Js.string ("Error:" ^ s)
  method next = errStream s
   end
)