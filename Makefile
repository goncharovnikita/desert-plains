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

build-example:
	stack exec desert-plains-exe -- --src ./test-data/index.desertp --dest ./test-data/index.html
