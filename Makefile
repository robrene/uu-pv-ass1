CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell symlinks

src/CCO/GCL/AG.hs : src/CCO/GCL/AG.ag src/CCO/GCL/AG/Base.ag
	uuagc -Hdcfws --self -P src/CCO/GCL src/CCO/GCL/AG.ag

haskell : src/CCO/GCL/AG.hs
	cabal configure $(CABAL-CONFIGURE-FLAGS)
	cabal build $(CABAL-BUILD-FLAGS)

symlinks : haskell
	mkdir -p bin
	ln -fs ../dist/build/parse-gcl/parse-gcl bin/

clean :
	rm -f src/CCO/GCL/AG.hs
	rm -rf dist
	rm -rf bin

repl : haskell
	cabal repl

sandbox:
	cabal sandbox init
	cabal install --dependencies-only

.PHONY : haskell clean sandbox
