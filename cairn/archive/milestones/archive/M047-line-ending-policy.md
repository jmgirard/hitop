# M047: One line-ending policy for the whole repository

**Status:** done (2026-08-22, PR #53 https://github.com/jmgirard/hitop/pull/53)

**Goal:** Every text file stored LF, every binary file declared not sniffed, and
a CI guard failing the push that reintroduces a CRLF.

**Outcome:** `.gitattributes` sets `* text=auto eol=lf` and declares `*.rda`,
`*.png`, `*.ico`, `*.docx`, `*.zip`, `*.qsf` binary, the byte-locked `-text`
trees last so they win; no path is left `unspecified` (was 284 of 332), and 18
text files renormalized byte-identically once CRs are stripped.
`data-raw/check_line_endings.R` fails on an undeclared path, a CR in a
convertible path, a binary left undeclared, and over-declaration of a text file,
reading blobs via one `git cat-file --batch` call with paths on stdin so no
filename reaches a command line; a `line-endings` CI job runs it. `air.toml`
gained `[format] line-ending = "lf"`, `DESIGN.md` a Line endings convention.

**Decisions:** M047-D1 — the blame-ignore SHA is appended at post-merge hygiene,
since a squash merge writes a commit the branch cannot name. Done and verified:
blame now resolves the renormalized files to their real authors.

**Review:** three-lens fan-out; blame-history and prior-review clean. Diff-bug
returned 14, all fixed; three returned the milestone — an apostrophe in a path
broke the guard's shell quoting so it read zero bytes and passed, no `git` call
checked its status, and a `CLAUDE.md` blame claim was false.
