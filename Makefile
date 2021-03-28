OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TESTS=$(shell find ./tests -name "*.ml" | sed s/ml/byte/g | sed s/.\\/tests\\///g )
MAIN=src/ocamlgrapher.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

define \n


endef

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) -tag 'debug' $(MAIN)

test:
	$(foreach TEST,$(TESTS),$(OCAMLBUILD) -I src -I tests -tag 'debug' tests/$(TEST) && echo RUNNING $(TEST) && ./$(TEST) -runner sequential ${\n})

docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private
