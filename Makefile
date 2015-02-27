CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell symlinks

haskell :
	cabal configure $(CABAL-CONFIGURE-FLAGS)
	cabal build $(CABAL-BUILD-FLAGS)

symlinks : haskell
	mkdir -p bin
	ln -fs ../dist/build/gcl/gcl bin/

clean :
	rm -rf dist
	rm -rf bin

repl : haskell
	cabal repl

sandbox:
	cabal sandbox init
	cabal install --dependencies-only

.PHONY : haskell clean sandbox
