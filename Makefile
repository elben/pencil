all: build docs
	@true

build:
	stack build --pedantic

docs:
	stack haddock

tags:
	@hasktags --ignore-close-implementation --ctags .

example-simple: build
	stack exec pencil-example-simple

example-blog: build
	stack exec pencil-example-blog

