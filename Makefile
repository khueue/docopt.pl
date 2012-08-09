PROLOG = swipl -O

.PHONY: all
all: prepare test

.PHONY: prepare
prepare:
	clear

.PHONY: test
test:
	@ echo "--- Run tests and exit ..."
	time $(PROLOG) -s load -g test -t halt

# TODO: Coverage doesn't seem to work at all. Fixed in newer SWI?
.PHONY: cov
cov:
	@ echo "--- Run tests, print test coverage and exit ..."
	$(PROLOG) -s load -g cov -t halt

.PHONY: repl
repl:
	@ echo "--- Load and enter REPL ..."
	$(PROLOG) -s load -g repl
