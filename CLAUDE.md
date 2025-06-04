# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains OCaml implementations of common data structures and algorithms (ODS) for educational purposes. It includes implementations of:

- Binary Search Trees
- Red-Black Trees
- Sorting Algorithms

The archive directory contains additional implementations that may be referenced or integrated into the main library.

## Build Commands

- `make build` - Clean and build the project
- `make clean` - Clean build artifacts
- `make test` - Run tests
- `make utop` - Start OCaml REPL with the library loaded

## Development Workflow

1. The project uses Dune as the build system
2. Main source files are in the `src/` directory
3. Tests are in the `test/` directory using OUnit framework
4. Archive contains older implementations for reference

## Code Architecture

- The code uses OCaml's module system and functors for abstraction
- Data structures are typically implemented as algebraic data types
- Heavy use of pattern matching for algorithm implementation
- Tree-based data structures follow recursive functional implementation patterns
- Self-balancing trees include appropriate rebalancing operations

## Running Tests

Tests are written using the OUnit framework:

- Run all tests: `make test`
- Run a specific test: `dune exec test/test_sort.exe`

## Environment Setup

Before development, ensure OCaml environment is properly set up:
```
eval $(opam env)
dune build
```