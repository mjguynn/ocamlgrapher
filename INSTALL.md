# Installation Instructions

### Prerequisites:
- OUnit2 must be installed through OPAM: `opam install -y ounit`
- bisect_ppx-ocamlbuild must be installed through OPAM: `opam install -y bisect_ppx bisect_ppx-ocamlbuild`

### Building the Program:
- Run `make build` to compile the program. Then run it with `./ocamlgrapher.byte`.
  Try `./ocamlgrapher.byte -h` for help. 
  OCamlGrapher *must* be run in the same folder as `graph_styles.css`.

  Note that quotes are not always necessary when inputting equations. 
  For example, `./ocamlgrapher.byte y=x` works, as does `./ocamlgrapher.byte "y=x`.
  However, it's a good idea to quote the equation strings no matter what;
  `./ocamlgrapher.byte "y=(x)"` works, but `./ocamlgrapher.byte y=(x)` does not!
  
### Running Tests
- To run the test suite, run `make test`.

### Making Documentation
- Run `make docs`. 
  The public docs will be in `_doc.public`, and the private docs will be in `_doc.private`.
