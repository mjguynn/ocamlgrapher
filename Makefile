PUBLIC=src/author.mli src/cmdline.mli src/common.ml src/config.mli src/defs.ml src/grapher.mli src/grid_lines.ml src/io.mli src/numericalmethods.mli src/parser.mli src/svghelpers.mli src/tokenizer.mli src/xmldom.ml
PRIVATE=src/cmdline.ml src/config.ml src/grapher.ml src/io.ml src/numericalmethods.ml src/ocamlgrapher.ml src/parser.ml src/svghelpers.ml src/tokenizer.ml
OBJECTS=$(MODULES:=.cmo)
TESTS=$(shell find ./tests -name "*.ml" | sed s/ml/byte/g | sed s/.\\/tests\\///g )
MAIN=src/ocamlgrapher.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'

define \n


endef

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) -I src -tag 'debug' $(MAIN)

test:
	$(foreach TEST,$(TESTS),$(OCAMLBUILD) -I src -I tests -tag 'debug' tests/$(TEST) && echo RUNNING $(TEST) && ./$(TEST) -runner sequential ${\n})

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
	$(foreach TEST,$(TESTS),BISECT_COVERAGE=YES $(OCAMLBUILD) -I src -I tests -tag 'debug' tests/$(TEST) && echo RUNNING $(TEST) && ./$(TEST) ${\n})

bisect: clean bisect-test
	bisect-ppx-report html