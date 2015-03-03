# Liberal weakest pre-condition transformer for GCL-like language

## Building

Requires cabal. The first command sets up a sandbox environment and installs all dependencies. The second command builds the project and creates synlinks to the executables in bin/.

    $ make sandbox
    $ make all

## Usage

    $ cat examples/example1.txt | ./bin/parse-gcl | ./bin/wlp | ./bin/expr2smt > examples/example1_z3.smt2
    $ z3 examples/example1_z3.smt2

Or in order to run Z3 immediately:

    $ z3 -smt2 <(cat examples/example1.txt | ./bin/parse-gcl | ./bin/wlp | ./bin/expr2smt)
