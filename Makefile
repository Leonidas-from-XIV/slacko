.PHONY: all
all:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: test
test:
	dune runtest --force
