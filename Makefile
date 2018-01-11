.PHONY: all
all:
	jbuilder build

.PHONY: clean
clean:
	jbuilder cleanr

.PHONY: test
test:
	jbuilder runtest
