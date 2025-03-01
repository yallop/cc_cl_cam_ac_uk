# Slang Compiler Explorer

## Install

```
# Get dependencies for compiling to javascript
opam install js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt

ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx \
          -linkpkg -o slang.byte ../slang/slang.ml

js_of_ocaml cubes.byte

cd ../web

```
