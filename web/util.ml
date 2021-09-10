(* Don't be afraid of this ugly type. It creates a stream like object in js.*)
type 'a stream = <
   steps : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Js_of_ocaml.Js.readonly_prop;
   next : 'b Js_of_ocaml.Js.meth
> Js_of_ocaml.Js.t as 'b
