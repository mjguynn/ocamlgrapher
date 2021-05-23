PUBLIC=src/author.mli src/cmdline.mli src/common.ml src/config.mli src/defs.ml src/grapher.mli src/graphstyles.mli src/io.mli src/numericalmethods.mli src/parser.mli src/svghelpers.mli src/tokenizer.mli src/xmldom.ml src/ocamlgrapher.ml 
PRIVATE=src/cmdline.ml src/config.ml src/grapher.ml src/graphstyles.ml src/io.ml src/numericalmethods.ml src/parser.ml src/svghelpers.ml src/tokenizer.ml
OBJECTS=$(MODULES:=.cmo)
MAIN=src/ocamlgrapher.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) -I src -tag 'debug' $(MAIN)

test:
	$(OCAMLBUILD) -I src -I tests -tag 'debug' tests/test.byte && ./test.byte -runner sequential

docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build/src -I src \
		-html -stars -d _doc.public $(PUBLIC)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build/src -I src \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(PUBLIC) $(PRIVATE)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private _coverage bisect*.coverage

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -I src -I tests -tag 'debug' tests/test.byte && ./test.byte

bisect: clean bisect-test
	bisect-ppx-report html