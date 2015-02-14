Build Status
=====

[![Build Status](https://travis-ci.org/hasufell/clac.png)](https://travis-ci.org/hasufell/clac)


About
=====
Very simple RPN calculator. Works with STDIN and arguments. Also has a gtk version.


Install
=======
clac is installed using cabal:
  $ cabal install

It works well in a sandbox too:
  $ cabal sandbox init && cabal install


Run
===
If your cabal binaries are in $PATH after installing, invoke clac like any
other program:
  $ clac-cli

It can also be ran via cabal (inside a sandbox or not):
  $ cabal run clac-cli

To run the gtk version:
  $ cabal run clac-gtk


Usage
=====
Straightforward RPN calculator.
  $ clac-cli 1 2 - 3 +
  $ echo 1 2 - 3 + | clac-cli
  $ clac-cli
    1 1 +^D

It supports multiple equations.
  $ clac-cli 1 1 + , 2 2 +

For available operators, check the operator list.
  $ clac-cli -o

To print the entire equation as a tree, run in verbose mode.
  $ clac-cli -v


TODO
====
  - Using something more appropriate than Double
  - Support for implicit multiple equations (currently only supports explicit)
  - Better pretty printing (intermediate calculations for instance)
