# Standards Check

## Goal

Review this repository against `STANDARDS.md`.
Make the smallest safe change that improves compliance.

## Inputs

- `AGENTS.md`
- `CLAUDE.md`
- `STANDARDS.md`
- `JOURNAL.md`, when present
- current git status
- current test and build files
- current README and docs

## Plan Mode

In plan mode:

1. Read `AGENTS.md`, `CLAUDE.md`, and `STANDARDS.md`.
2. Compare the repository against each standard.
3. Report which standards pass, fail, or need human review.
4. Name one smallest safe change for execute mode.
5. Do not edit files.
6. Do not create a branch.
7. Do not open a pull request.

## Execute Mode

In execute mode:

1. Read `AGENTS.md`, `CLAUDE.md`, and `STANDARDS.md`.
2. Pick one small fix that does not need human review.
3. Create a non-default branch named `factory/standards-check-<short-description>`.
4. Make the change.
5. Run `make test` when the OCaml toolchain is available.
6. Commit the change.
7. Push the branch.
8. Open a draft pull request.
9. Include what changed, what was checked, and any remaining gaps.

## Stop Rules

Stop and report `blocked` if:

- the standard requires a human product or license decision
- tests cannot run because required dependencies, secrets, or services are missing
- the fix would change public behavior beyond the workflow scope
- the working tree already has unrelated user changes
- the repo has no clear default branch or remote

## Safety

- Never merge a pull request.
- Never push to `master`.
- Never do broad cleanup.
- Never change the license without human review.
- Prefer one small pull request over many unrelated fixes.
