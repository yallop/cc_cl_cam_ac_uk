# cc2020_cl_cam_ac_uk

[![Main workflow](https://github.com/yallop/cc_cl_cam_ac_uk/actions/workflows/test.yml/badge.svg)](https://github.com/yallop/cc_cl_cam_ac_uk/actions/workflows/test.yml)

## Ocaml dependencies

After installing opam, run:

```
opam install dune ppx_deriving_yojson js_of_ocaml js_of_ocaml-ppx
```

## slang-cli

To build the slang compiler:

```
dune build
```

See [the slang readme file](slang/README.txt) for instructions on how to run the compiler.

To clean the repo

```
dune clean
```
