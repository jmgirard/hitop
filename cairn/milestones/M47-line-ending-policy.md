# M47: One line-ending policy for the whole repository

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Driving RR:** —
- **Branch/PR:** —

## Goal

Every tracked text file is stored LF, every binary file is declared binary
rather than left to git's content sniff, and a CI guard fails the push that
reintroduces a CRLF.

## Scope

Surface tier: **internal** — the deliverable is the repository's line-ending
policy and the stored bytes of its source files. No external consumer of the
package relies on either; the criteria below bind the distributed artifacts to
stay unchanged rather than to change.

**In:** a `* text=auto eol=lf` rule in `.gitattributes` plus explicit `binary`
declarations for every family that must never be converted (`*.rda`, `*.png`,
`*.ico`, and the two artifact directories already declared `-text`);
`git add --renormalize` over the 14 tracked text files still stored CRLF —
`R/generate_qualtrics.R`, `R/reliability.R`, eight `data-raw/*.csv`,
`data-raw/hitopsr.qmd`, `data-raw/.gitignore`, `devel/titanium.R`, and two
`.vscode/*.json`; a committed script that walks `git ls-files` and reports each
path's `git check-attr text` value against its blob's bytes; a CI step running
that script; and a `.git-blame-ignore-revs` file naming the normalization
commit, with the activating `git config` line documented in `CLAUDE.md`.

**Out:** converting any file to CRLF, or changing any byte under
`inst/extdata/`, `pkgdown/assets/downloads/`, `data/`, or `R/sysdata.rda` —
AC3 proves none moved, and a change there would need a `hitop_artifacts`
manifest row under D-016. Reformatting, restyling, or re-indenting the
renormalized files → out; this milestone changes line endings and nothing
else, which is what keeps AC3 checkable. An `.editorconfig` or a pre-commit
hook → ROADMAP candidate row if a CRLF is ever reintroduced despite the CI
guard. Declaring `*.svg` binary → out and barred:
`pkgdown/favicon/favicon.svg` is genuine text and the policy must renormalize
it like any other.

## Acceptance criteria

`<base>` below is the merge-base commit this milestone's branch was cut from.

- [ ] AC1 — On a clean checkout at the merge commit,
      `git add --renormalize .` followed by `git status --porcelain` reports no
      output. The renormalize pass visits every tracked path, so it is itself
      the procedure that enumerates the domain: any file whose stored bytes
      disagree with the declared policy is staged and appears here.
- [ ] AC2 — A committed script walks `git ls-files` and reports, for every
      tracked path, its `git check-attr text` value and whether its index blob
      contains a CR byte. No path that `git check-attr` resolves as `text=auto`
      carries a CR byte in its index blob.
- [ ] AC3 — `git diff --name-only <base> HEAD` lists only `.gitattributes`,
      the files this milestone adds, the tracking files it updates, and the
      tracked text paths the policy renormalizes; no other tracked path
      appears. Every committed artifact's md5 still equals its current
      `hitop_artifacts` row, with `tests/testthat/test-artifacts.R` green.
- [ ] AC4 — The same script reports, for every tracked path, whether its index
      blob contains a NUL byte within its first 8000 bytes — git's own binary
      criterion — beside its `git check-attr text` value. No path meeting that
      criterion resolves as `text=auto`, so no binary file's stored bytes
      depend on the content sniff at checkout.
- [ ] AC5 — The committed guard reports a failure when a CR byte is present in
      any path `git check-attr` resolves as `text=auto`, and reports none on
      the clean tree.
- [ ] AC6 — `devtools::document()` produces no diff, and `devtools::test()`
      and `devtools::check()` are clean (0 errors, 0 warnings, and no note
      absent from the pre-milestone baseline of the default branch).

## Coverage

- AC1 → T1, T2
- AC2 → T3, T6
- AC3 → T2, T6
- AC4 → T3, T6
- AC5 → T4, T6
- AC6 → T6

## Tasks

- [ ] T1 — Write `.gitattributes`: `* text=auto eol=lf`, explicit `binary` for
      `*.rda`, `*.png`, `*.ico`, keeping the existing `inst/extdata/** -text`
      and `pkgdown/assets/downloads/** -text` lines and their D-016/D-033
      comments. Do not declare `*.svg` binary — `pkgdown/favicon/favicon.svg`
      is text.
- [ ] T2 — Run `git add --renormalize .`, commit, and confirm by
      `git diff --name-only` that only the 14 expected text paths plus
      `.gitattributes` moved. Re-run renormalize to confirm idempotence.
- [ ] T3 — Write `data-raw/check_line_endings.R`: walk `git ls-files`,
      report per path the `git check-attr text` value, CR presence, and NUL
      within the first 8000 bytes; exit non-zero on any `text=auto` path with
      a CR, or any NUL-carrying path resolving `text=auto`.
- [ ] T4 — Add a step running that script to the existing
      `.github/workflows/` check workflow. It shells out to `git`, which no
      CRAN check environment or built tarball provides, so it is a CI step and
      not a testthat test.
- [ ] T5 — Add `.git-blame-ignore-revs` naming T2's commit, plus a `CLAUDE.md`
      line giving the `git config blame.ignoreRevsFile .git-blame-ignore-revs`
      activation. Add `.Rbuildignore` entries for any new top-level file that
      `devtools::check()` NOTEs.
- [ ] T6 — Run `devtools::document()`, `devtools::test()`, and
      `devtools::check()`; record the AC2/AC4 script output for the Review.

## Work log

- 2026-08-22: created by /milestone-plan, promoting the CRLF candidate row added 2026-08-22 from M45's review finding 5. Its promotion condition — "when a milestone next touches `generate_qualtrics.R` or `reliability.R`" — was met by M46, which had to hand-restore that file's CRLF endings mid-implementation to stop a 5-line edit presenting as a 553-line diff.
- 2026-08-22: plan-gate criteria audit ran in REDUCED mode (internal tier) by a fresh-context [O] reader that authored none of the wording; it returned six findings and all six were fixed here. A criterion promising the Windows CI job passes was an instrument property spanning an environment boundary, which the internal-tier standard bars, and is replaced by AC4's locally decidable claim. AC2 stated its promise against `.gitattributes` — an exemption registry — and its biconditional was factually false in reverse, since 12 declared-`-text` artifacts carry no CR byte at all. AC3 named a hand-list of seven paths as a proxy for "every byte-locked artifact" and the list already omitted `devel/example.png`; it is now a claim about a whole-repo diff. AC2 and AC5 also bound evidence-recording and demonstration protocol rather than the deliverable, and those clauses moved to the tasks.
- 2026-08-22: the same audit verified empirically in a throwaway clone that `git add --renormalize .` under this policy converges in one pass, is idempotent, and alters nothing under `inst/extdata/`, `pkgdown/assets/downloads/`, `data/`, `R/sysdata.rda`, or any image path; and that `data/*.rda`, `R/sysdata.rda`, and every PNG/ICO resolve as `text=auto` under a bare policy, saved today only by git's NUL-byte sniff. It also found `pkgdown/favicon/favicon.svg` is genuine text, so a naive `*.svg binary` declaration would corrupt it — now barred in Scope.
- 2026-08-22: the user declined the plan question gate and asked to resume, so the three open choices were settled autonomously and are recorded as an override; each is falsifiable on the evidence class named in the lines below.
- 2026-08-22: plan chose normalizing every tracked text file, `data-raw/`'s keying-provenance CSVs included, over normalizing only `R/` and declaring the rest an exception, because two conventions in one repo is what let this recur and AC3 proves no parsed value or built dataset moved; falsified by a keying-provenance audit that needs `git blame` on those CSVs to reach past the normalization commit.
- 2026-08-22: plan chose committing `.git-blame-ignore-revs` over accepting the blame rewrite because `R/generate_qualtrics.R` is entirely CRLF and would otherwise re-attribute every line to one commit; falsified by the file proving useless in practice, since it takes a per-clone `git config` that a fresh clone does not inherit.
- 2026-08-22: plan chose a CI workflow step for the guard over a testthat test because the check shells out to `git`, which no CRAN check environment or built tarball provides, so a test would have to skip in exactly the places a guard is claimed to run; falsified by a CRLF reaching the default branch through a path CI does not run.

## Decisions

## Review
