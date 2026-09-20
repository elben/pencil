all: build test docs
	@true

build:
	cabal build all

haddock:
	cabal haddock

tags:
	@hasktags --ctags .

# Make as .PHONY so that make doesn't interpret the docs/ folder as the build
# result of this `docs` target.
docs:
	rm -rf docs/*
	cabal test pencil-docs
.PHONY: docs

# Full doctests over all of src. Requires `cabal install doctest --ignore-project`
# so that `doctest` is on PATH.
doctest:
	cabal repl --with-compiler=doctest --repl-options='-w -Wdefault'

test: doctest
	cabal test

example-simple:
	rm -rf examples/Simple/out/*
	cabal test pencil-example-simple

example-blog:
	rm -rf examples/Blog/out/*
	cabal test pencil-example-blog

example-complex:
	rm -rf examples/Complex/out/*
	cabal test pencil-example-complex

candidate:
	cabal check
	cabal sdist
