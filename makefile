.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: test
test:
	stack test airput --fast


.PHONY: install
install:
	stack build airput \
		--fast \
		--copy-bins


.PHONY: clean
clean:
	stack purge
