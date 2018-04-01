.PHONY: all
all:
	jbuilder build

.PHONY: clean
clean:
	jbuilder clean

.PHONY: test
test:
	jbuilder runtest
