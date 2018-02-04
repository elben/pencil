all: build docs
	@true

build:
	stack build --pedantic

docs:
	stack haddock

