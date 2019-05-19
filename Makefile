all: build test docs
	@true

shell:
	nix-shell --attr env

build:
	cabal build

haddock:
	cabal haddock

tags:
	@hasktags --ctags .

docs:
	rm -rf docs/*
	cabal test pencil-docs

test:
	cabal test
	doctest src/

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
	FILENAME=`cabal sdist | grep "Source tarball created" | awk '{ print $4 }'`
	# echo ${$FILENAME}
	# cabal upload ${FILENAME}
