About
=====
Very simple CLI RPN calculator. Works with STDIN and arguments.


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
  $ clac

It can also be ran via cabal (inside a sandbox or not):
  $ cabal run


Usage
=====
Straightforward RPN calculator.
  $ clac 1 2 - 3 +
  $ echo 1 2 - 3 + | clac
  $ clac
    1 1 +^D

It supports multiple equations.
  $ clac 1 1 + , 2 2 +

For available operators, check the operator list.
  $ clac -o

To print the entire equation as a tree, run in verbose mode.
  $ clac -v


TODO
====
If you want to help, these things should probably be added to clac:
  - Using something more appropriate than Double
  - Support for implicit multiple equations (currently only supports explicit)
  - Better pretty printing (intermediate calculations for instance)
