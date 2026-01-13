all: build

.PHONY: clean
clean:
	@dune clean

.PHONY: build
build: clean
	@dune build

.PHONY: utop
utop:
	@dune utop src

.PHONY: test
test: clean
	@dune runtest
