.PHONY: all
all:
	jbuilder build --dev

.PHONY: clean
clean:
	jbuilder clean

.PHONY: test
test:
	jbuilder runtest --force
