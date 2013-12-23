OCBFLAGS := -classic-display
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top
all: odslib.cma odslib.cmxa ods.native
debug: all ods.cma

%.cma:
	$(OCB) $@
%.cmxa:
	$(OCB) $@
%.native:
	$(OCB) $@

clean:
	$(OCB) -clean
	$(RM) src/version.ml*

top: debug
	ocaml
