.PHONY: all bench build clean configure haddock hpc install repl run test

all: install configure build haddock test hpc bench

bench:
	cabal bench

build:
	cabal build

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-benchmarks --enable-tests

haddock:
	cabal haddock --hyperlink-source
	# dist/doc/html/${packageName}/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	# tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-benchmarks --enable-tests --only-dependencies --reorder-goals

repl:
	cabal repl lib:${packageName}

run:
	cabal run ${packageName}

test:
	cabal test

