.PHONY: test build install run lint

test:
	stack test

install:
	stack install

build:
	stack build

run:
	stack exec desert-plains-exe

lint:
	hlint .