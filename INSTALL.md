# Installation Instructions

### Prerequisites:
- OUnit2 must be installed through OPAM: `opam install -y ounit"
- bisect_ppx-ocamlbuild must be installed through OPAM: `opam install -y bisect_ppx bisect_ppx-ocamlbuild`

### Building the Program:
- Run `make build` to compile the program. Then run it with `./ocamlgrapher.byte`.
  Try `./ocamlgrapher.byte -h` for help. 
  OCamlGrapher *must* be run in the same folder as `graph_styles.css`.
  
### Running Tests
- To run the test suite, run `make test`.
