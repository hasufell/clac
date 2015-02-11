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

For available operators, check the help.
  $ clac help


TODO
====
If you want to help, these things should probably be added to clac:
  - Using Scientific (or something else appropriate) instead of Double
  - Support for multiple equations
  - Pretty printing
