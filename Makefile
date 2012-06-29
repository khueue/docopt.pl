PROLOG = swipl -O

.PHONY: all
all: trim test

# Remove trailing whitespace and such. Not important.
.PHONY: trim
trim:
	clear
	@- trim *.md *.pl* src/*.pl*

.PHONY: test
test:
	@ echo "--- Run tests and exit ..."
	time $(PROLOG) -s load -g test -t halt

.PHONY: repl
repl:
	@ echo "--- Load and enter REPL ..."
	$(PROLOG) -s load -g repl
