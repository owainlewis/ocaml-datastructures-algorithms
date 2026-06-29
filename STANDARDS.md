# STANDARDS.md

This file defines the repository standards that Factory agents should enforce.

## Project Purpose

The repository should remain an educational OCaml library of data structures and algorithms.
Examples, names, and tests should help readers understand the implementation.

## Required Files

- `LICENSE` must exist.
- `README.md` must explain the project purpose, build commands, test commands, usage, and project structure.
- `CLAUDE.md` or `AGENTS.md` must explain how agents should work in the repo.
- `dune-project` and `ods.opam` must stay aligned with the build system.

## License

- License metadata in `README.md`, `LICENSE`, and `ods.opam` must agree.
- Changing the license requires human review.

## OCaml Code

- Code should be readable and idiomatic OCaml.
- Prefer small functions and clear pattern matching.
- Keep public module names stable unless a human approves an API change.
- Avoid broad rewrites when a targeted fix is enough.

## Tests

- Code changes should include or update tests when behavior changes.
- `make test` should pass before opening a pull request when the local OCaml toolchain is available.
- If tests cannot run locally, the pull request must explain why.

## Documentation

- README examples should compile or clearly be marked as pseudo-code.
- Document any new data structure or algorithm in `README.md`.
- Keep docs aligned with files that actually exist in the repo.

## Pull Requests

- Use a non-default branch.
- Open draft pull requests for agent-created changes.
- Include a short summary, verification steps, and remaining gaps.
- Never merge pull requests automatically.
