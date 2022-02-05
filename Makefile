.PHONY: test build install run

test:
	stack test

install:
	stack install

build:
	stack build

run:
	stack exec desert-plains-exe
