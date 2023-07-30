# We use Stack for Kellisp builds
# Here, we use a phony makefile for ease of access

# primary build, used for quick iteration
.PHONY: all
all:
	stack build --fast

# used for more complete builds, with optimization
.PHONY: build
build:
	stack build

# builds and subsequently runs the REPL
.PHONY: repl
repl:
	stack build
	stack exec kellisp-exe

# generates documentation for build / dependencies
# NOTE: maybe later, pass along arguments
.PHONY: docs
docs:
	stack haddock

# used to run test suite
.PHONY: test
test:
	stack test --fast

# cleans out any temp files, executables, etc
.PHONY: clean
clean:
	stack clean
