# M47: One line-ending policy for the whole repository

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Driving RR:** —
- **Branch/PR:** `m47-line-ending-policy` / [PR #53](https://github.com/jmgirard/hitop/pull/53)

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
`git add --renormalize` over the 18 tracked text files still stored CRLF —
`R/generate_qualtrics.R`, `R/reliability.R`, eleven `data-raw/*.csv`,
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

- [x] AC1 — On a clean checkout at the merge commit,
      `git add --renormalize .` followed by `git status --porcelain` reports no
      output. The renormalize pass visits every tracked path, so it is itself
      the procedure that enumerates the domain: any file whose stored bytes
      disagree with the declared policy is staged and appears here.
- [x] AC2 — A committed script walks `git ls-files` and reports, for every
      tracked path, its `git check-attr text` value and whether its index blob
      contains a CR byte. No path that `git check-attr` resolves as `text=auto`
      carries a CR byte in its index blob.
- [x] AC3 — `git diff --name-only <base> HEAD` lists only `.gitattributes`,
      the files this milestone adds, the tracking files it updates, and the
      tracked text paths the policy renormalizes; no other tracked path
      appears. Every committed artifact's md5 still equals its current
      `hitop_artifacts` row, with `tests/testthat/test-artifacts.R` green.
- [x] AC4 — The same script reports, for every tracked path, whether its index
      blob contains a NUL byte within its first 8000 bytes — git's own binary
      criterion — beside its `git check-attr text` value. No path meeting that
      criterion resolves as `text=auto`, so no binary file's stored bytes
      depend on the content sniff at checkout.
- [x] AC5 — The committed guard reports a failure when a CR byte is present in
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

- [x] T1 — Write `.gitattributes`: `* text=auto eol=lf`, explicit `binary` for
      `*.rda`, `*.png`, `*.ico`, keeping the existing `inst/extdata/** -text`
      and `pkgdown/assets/downloads/** -text` lines and their D-016/D-033
      comments. Do not declare `*.svg` binary — `pkgdown/favicon/favicon.svg`
      is text.
- [x] T2 — Run `git add --renormalize .`, commit, and confirm by
      `git diff --name-only` that only the 14 expected text paths plus
      `.gitattributes` moved. Re-run renormalize to confirm idempotence.
- [x] T3 — Write `data-raw/check_line_endings.R`: walk `git ls-files`,
      report per path the `git check-attr text` value, CR presence, and NUL
      within the first 8000 bytes; exit non-zero on any `text=auto` path with
      a CR, or any NUL-carrying path resolving `text=auto`.
- [x] T4 — Add a step running that script to the existing
      `.github/workflows/` check workflow. It shells out to `git`, which no
      CRAN check environment or built tarball provides, so it is a CI step and
      not a testthat test.
- [x] T5 — Add `.git-blame-ignore-revs` naming T2's commit, plus a `CLAUDE.md`
      line giving the `git config blame.ignoreRevsFile .git-blame-ignore-revs`
      activation. Add `.Rbuildignore` entries for any new top-level file that
      `devtools::check()` NOTEs.
- [x] T6 — Run `devtools::document()`, `devtools::test()`, and
      `devtools::check()`; record the AC2/AC4 script output for the Review.

## Work log

- 2026-08-22: created by /milestone-plan, promoting the CRLF candidate row added 2026-08-22 from M45's review finding 5. Its promotion condition — "when a milestone next touches `generate_qualtrics.R` or `reliability.R`" — was met by M46, which had to hand-restore that file's CRLF endings mid-implementation to stop a 5-line edit presenting as a 553-line diff.
- 2026-08-22: plan-gate criteria audit ran in REDUCED mode (internal tier) by a fresh-context [O] reader that authored none of the wording; it returned six findings and all six were fixed here. A criterion promising the Windows CI job passes was an instrument property spanning an environment boundary, which the internal-tier standard bars, and is replaced by AC4's locally decidable claim. AC2 stated its promise against `.gitattributes` — an exemption registry — and its biconditional was factually false in reverse, since 12 declared-`-text` artifacts carry no CR byte at all. AC3 named a hand-list of seven paths as a proxy for "every byte-locked artifact" and the list already omitted `devel/example.png`; it is now a claim about a whole-repo diff. AC2 and AC5 also bound evidence-recording and demonstration protocol rather than the deliverable, and those clauses moved to the tasks.
- 2026-08-22: the same audit verified empirically in a throwaway clone that `git add --renormalize .` under this policy converges in one pass, is idempotent, and alters nothing under `inst/extdata/`, `pkgdown/assets/downloads/`, `data/`, `R/sysdata.rda`, or any image path; and that `data/*.rda`, `R/sysdata.rda`, and every PNG/ICO resolve as `text=auto` under a bare policy, saved today only by git's NUL-byte sniff. It also found `pkgdown/favicon/favicon.svg` is genuine text, so a naive `*.svg binary` declaration would corrupt it — now barred in Scope.
- 2026-08-22: the user declined the plan question gate and asked to resume, so the three open choices were settled autonomously and are recorded as an override; each is falsifiable on the evidence class named in the lines below.
- 2026-08-22: plan chose normalizing every tracked text file, `data-raw/`'s keying-provenance CSVs included, over normalizing only `R/` and declaring the rest an exception, because two conventions in one repo is what let this recur and AC3 proves no parsed value or built dataset moved; falsified by a keying-provenance audit that needs `git blame` on those CSVs to reach past the normalization commit.
- 2026-08-22: plan chose committing `.git-blame-ignore-revs` over accepting the blame rewrite because `R/generate_qualtrics.R` is entirely CRLF and would otherwise re-attribute every line to one commit; falsified by the file proving useless in practice, since it takes a per-clone `git config` that a fresh clone does not inherit.
- 2026-08-22: plan chose a CI workflow step for the guard over a testthat test because the check shells out to `git`, which no CRAN check environment or built tarball provides, so a test would have to skip in exactly the places a guard is claimed to run; falsified by a CRLF reaching the default branch through a path CI does not run.

- 2026-08-22: minor reorder — T3's check script was written before T1/T2 so it could show the before-state, which is the tests-first order; task numbering is unchanged.
- 2026-08-22: T3 — `data-raw/check_line_endings.R` written. Its first version passed vacuously on a repo carrying 84 CR-bearing files: `git check-attr` reports `unspecified`, not `auto`, where nothing declares the attribute, so comparing against `"auto"` classified all 332 tracked paths as declared-safe. The classifier now treats `unspecified` as its own violation — behavior there falls back to the local `core.autocrlf` — which made the pre-policy run exit 1 on 284 undeclared paths.
- 2026-08-22: T1/T2 — `.gitattributes` gained `* text=auto eol=lf` plus `binary` for `*.rda`, `*.png`, `*.ico`, `*.docx`, `*.zip`, keeping the two `-text` directories last so they win. `.svg` is deliberately excluded: `pkgdown/favicon/favicon.svg` is text. `git add --renormalize .` staged 18 text files, and a second pass staged nothing new. Verified: no path under `inst/extdata/`, `pkgdown/assets/downloads/`, `data/`, `R/sysdata.rda`, `man/figures/`, `pkgdown/favicon/`, `hitop_hex.png`, or `devel/example.png` was staged; all 18 renormalized files show an empty `git diff -w` and are byte-identical to their `main` blobs once CRs are stripped. Post-policy the checker reports 0 undeclared paths, 254 `text=auto`, 78 declared, and the 66 CR-carrying files are exactly the 66 git counts binary.
- 2026-08-22: plan correction — Scope said "the 14 tracked text files still stored CRLF ... eight `data-raw/*.csv`". The observed set is 18 files and 11 CSVs; `git add --renormalize .` is the authority and the Scope enumeration was a miscount. Corrected in place rather than convened at a gate: no acceptance criterion cites a count, and the set being described is unchanged.
- 2026-08-22: T4 — a separate `line-endings` job added to `.github/workflows/R-CMD-check.yaml`, on ubuntu only, because the script reads each path's index blob and no checkout-time conversion can alter what it sees. YAML re-parsed to confirm two jobs.
- 2026-08-22: T5 discovery — a `.git-blame-ignore-revs` naming this branch's normalization commit would be inert. Verified against M46: its branch tip `0264d7d` is not an ancestor of `main`, because a squash merge writes a new commit. The file, its activation instructions, and the `CLAUDE.md` line therefore land on the branch, and the squashed commit's SHA is appended during post-merge hygiene.
- 2026-08-22: AC5 evidence — the guard was mutation-tested at two path types by writing CRLF blobs straight into the index with `git hash-object -w --no-filters` plus `git update-index`. A plain `git add` cannot reproduce the fault at all, because `eol=lf` normalizes on the way in; the low-level write is the shape a merge from a pre-policy branch takes. The guard exited 1 naming `R/reliability.R`, then 1 naming both it and `data-raw/norms_pid5_ors.csv`, and 0 once both were renormalized.
- 2026-08-22: T6 first `devtools::check()` run raised exactly the NOTE T5 anticipated — "Found the following hidden files and directories: .git-blame-ignore-revs" — so `^\\.git-blame-ignore-revs$` was added to `.Rbuildignore` and the check re-run. `devtools::document()` produces no diff. The full branch diff against the base lists 25 paths: `.gitattributes`, the two files added, the two edited, the two tracking files, and the 18 renormalized text paths; no artifact, dataset, or image path appears.
- 2026-08-22: verify slot clean — `devtools::test()` FAIL 0 / WARN 0 / SKIP 1 / PASS 13778 (the skip is the pre-existing SDTD item-38 keying dispute); `devtools::check()` Status OK with 0 errors, 0 warnings, 0 notes after the `.Rbuildignore` entry; `devtools::document()` no diff; the guard exits 0 reporting 334 tracked paths, 0 undeclared, and 66 CR-carrying files all of which git counts binary. All tasks done; status to review.
- 2026-08-22: checkpoint — task boxes stay unticked until `devtools::test()` and `devtools::check()` report; the run was still in flight when this landed.

## Decisions

## Review

_Fresh evidence gathered 2026-08-22. AC1-AC5 were executed in a throwaway
clone of the branch (`git clone --branch m47-line-ending-policy`), because
`git add --renormalize` and the mutation probes write to an index and the live
tree must not be disturbed mid-review. `<base>` is `70c5b50`._

- **AC1 — verified.** On a clean checkout of the branch tip,
  `git add --renormalize .` followed by `git status --porcelain` printed
  nothing at all (0 lines). A second pass printed nothing either, so the
  policy is idempotent and the stored bytes already agree with it.
- **AC2 — verified.** `data-raw/check_line_endings.R` reports 334 tracked
  paths: 256 resolving `text=auto`, 78 declared `-text`/`binary`, and 0
  undeclared. No path resolving `text=auto` carries a CR byte. Re-derived
  independently of the script, by piping `git ls-files` through
  `git check-attr --stdin text`, selecting the 256 convertible paths, and
  reading each one's blob with `git cat-file`: 0 of them carry a CR byte.
- **AC3 — verified.** `git diff --name-only 70c5b50 HEAD` lists 26 paths and
  classifies exactly: 1 policy file (`.gitattributes`), 2 added
  (`data-raw/check_line_endings.R`, `.git-blame-ignore-revs`), 3 edited
  (`.Rbuildignore`, `CLAUDE.md`, `.github/workflows/R-CMD-check.yaml`), 2
  tracking files, and the 18 renormalized text paths. Restricting the same
  diff to `inst/extdata/`, `pkgdown/assets/downloads/`, `data/`,
  `R/sysdata.rda`, `man/figures/`, `pkgdown/favicon/`, `hitop_hex.png`, and
  `devel/example.png` returns nothing. `test-artifacts.R` passes 121
  assertions, so every artifact's md5 still equals its `hitop_artifacts` row.
- **AC4 — verified.** The script reports 66 paths carrying a NUL byte within
  their first 8000 — git's own binary criterion — and all 66 resolve
  `-text`/`binary`, none `text=auto`. Re-derived independently by the same
  `git check-attr` + `git cat-file` walk: 66 NUL-carrying paths, 0 of them
  left convertible.
- **AC5 — verified, beyond what the criterion asks.** Control: the guard
  exits 0 on the clean tree. Mutation A — a CRLF blob written straight into
  the index for `R/reliability.R` via `git hash-object -w --no-filters` plus
  `git update-index` — exit 1, naming `R/reliability.R` under "convertible
  paths carrying a CR byte". Mutation B, the same at a second path type
  (`data-raw/hitopbr_items.csv`) — exit 1, naming both. Mutation C, deleting
  the `*.rda binary` declaration — exit 1 under "binary paths not declared",
  naming `R/sysdata.rda` and the `data/*.rda` files, and the declared count
  falling 78 to 56. Restored control: exit 0. The low-level index write is
  the only way to produce mutation A or B at all: `eol=lf` normalizes on
  `git add`, so an ordinary commit cannot reintroduce a CR — the fault shape
  the CR half of the guard actually defends against is a merge from a branch
  predating the policy.

### Consistency gate

- `cairn_validate.py` exit 0, every check PASS, 20 advisory warnings all
  pre-existing dangling `D-001`..`D-012` tokens from the pre-migration
  numbering range.
- No `DESIGN.md` principle changed, so `cairn_impact.py` was not run.
- Profile `r-package` toolchain slot: `devtools::document()` no diff;
  `README.Rmd`/`README.md` untouched by the diff; `pkgdown::check_pkgdown()`
  reports no problems; the one new top-level file `.git-blame-ignore-revs`
  carries its `.Rbuildignore` entry, added after the first `check()` run
  raised the hidden-file NOTE naming it. **No `NEWS.md` entry is owed**: the
  milestone changes stored line endings, a `data-raw/` script, and CI, and
  no package behavior a user can observe.

### Independent review

