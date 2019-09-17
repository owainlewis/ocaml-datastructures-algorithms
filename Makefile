all: build

.PHONY: clean
clean:
	@dune clean

.PHONY: build
build: clean
	@dune build
