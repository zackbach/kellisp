# Kellisp

Kellisp, from "Has*kell Lisp*", is a simple Lisp implementation in Haskell.

This project is based upon [Make a Lisp](https://github.com/kanaka/mal)
and [Write Yourself a Scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
([V2](https://wespiser.com/writings/wyas/00_overview.html))

## Features
TODO: update, add documentation too

## Using Kellisp
To build, use any of the [Stack build commands](https://docs.haskellstack.org/en/stable/GUIDE/#the-build-synonyms)
```
> stack build
```

or alternatively use the Makefile commands:
```
> make

> make build

> make test

> make clean
```

To launch a repl, use
```
> stack exec kellisp-exe [-- filename]
```
after building. If a filename is specified, the contents will be loaded into the repl.

To build and subsequently launch a repl, the following Makefile commands are provided:
```
> make repl

> make load file=filename
```
