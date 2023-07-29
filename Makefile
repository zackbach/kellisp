# We use Stack for Kellisp builds
# Here, we use a phony makefile for ease of access

# primary build, used for quick iteration
.PHONY: all
all:
	stack build --fast

# used for more complete builds, with docs and optimization
.PHONY: build
build:
	stack build --haddock

# used to run test suite
.PHONY: test
test:
	stack test --fast --haddock

# cleans out any temp files, executables, etc
.PHONY: clean
clean:
	stack clean
