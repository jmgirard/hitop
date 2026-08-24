# M050: Naming and response controls for the builder's online exports

- **Status:** review
- **Priority:** normal
- **Depends on:** M049
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** `m050-online-export-controls` / https://github.com/jmgirard/hitop/pull/56

## Goal

A researcher can name what the builder's Qualtrics and REDCap exports create —
the survey block, the collected column prefix, the REDCap form — and choose
whether REDCap marks items required, with the package guarding those arguments
instead of writing whatever it is handed straight into the artifact.

## Scope

**User-facing** on both sides: four controls on a public web page, and a
package behavior change every R caller sees.

**In (package):** guards on every formal of `generate_qualtrics_hitopsr()` and
`generate_redcap_hitopsr()` that reaches the built artifact — `block_name`,
`id_prefix`, `include_instructions`, `breaks`, `form_name`, `required` — none
of which is checked today (`R/generate_qualtrics.R:83-104`,
`R/generate_redcap.R:84-104`); the existing `validate_string()` /
`validate_flag()` / `validate_count()` helpers in `R/util.R`, noting that
`validate_count()` rejects `0` while `breaks` documents `0`/`NULL` as
"disable pagination". The guards land in the shared `build_qualtrics_txt()` /
`build_redcap_zip()` builders, so the HiTOP-BR and PID-5 generators are swept
with them; `generate_redcap_hitophsum()`, which builds its dictionary without
those builders, gets the same `form_name` / `required` guards directly.
Tests, NEWS.

**In (builder):** four controls — Qualtrics block name, Qualtrics ID prefix,
REDCap form name, REDCap required — each passed to its own format's call and
no other, and each free-text value reaching R through
`webR.objs.globalEnv.bind()`, never string interpolation; a README section.

**Out:** `breaks` and `include_instructions` app controls → a new candidate
row (their package-side guards still land here, since both are formals of the
functions being touched). Target-system *format* rules — a REDCap-invalid
`form_name`, a spaced `id_prefix` — → a new candidate row: no citable REDCap
or Qualtrics naming spec is on the `references/` shelf, and IP2 bars inventing
one. Word-form controls → M049. Download naming → the standing M048 candidate
row. `title`, `font_size`, `font_family` → a new candidate row.

## Acceptance criteria

- [x] AC1. Every formal of `generate_qualtrics_hitopsr()` and
      `generate_redcap_hitopsr()` other than `file`, `module`, and `subset`
      rejects a wrong-type value with a `cli` error naming that argument. The
      domain is enumerated by a test that walks `formals()` of the two
      functions and skips only those three by name, so a formal added later is
      swept without editing the test. Evidence: the test source and its
      passing output.
- [x] AC2. The guards changed no output. Rebuilding at defaults,
      `hitopsr_qualtrics.txt` matches the committed `inst/extdata/` copy by
      md5, and the REDCap archive's `instrument.csv` matches the committed
      archive's member content. Evidence: both comparisons' output.
- [x] AC3. In a browser loading the modified page, all four controls drive
      their downloads. A two-scale module downloaded with a non-default block
      name, ID prefix, and form name yields a Qualtrics `.txt` whose
      `[[Block:...]]` line and `[[ID:...]]` question IDs carry the supplied
      strings, and a REDCap `instrument.csv` whose `Form Name` column carries
      the supplied form name; with *required* unticked that file's
      `Required Field?` column reads `n` on every item row and with it ticked
      reads `y` on every item row, row count stated. With every control left
      at its default, the same module's Qualtrics `.txt` is byte-identical,
      and its REDCap `instrument.csv` identical, to what the currently
      deployed page produces. Evidence: the strings and column values quoted
      from each download, and the default-state comparison.
- [x] AC4. Free text reaches R only through `bind()`, never interpolation: a
      block name containing a double quote, a backslash, and the sequence `")`
      produces a Qualtrics download whose block line carries that string
      verbatim, with no error reported on the page and no truncation.
      Evidence: the input string and the block line from the download.
- [x] AC5. The builder `README.md` gains a section naming what each of the
      four controls sets in its target system, and stating that a name the
      target system rejects surfaces at import time rather than at generation.
      Evidence: the section quoted, carrying the verification date.
- [x] AC6. NEWS.md records that these arguments now error on a wrong-type
      value where they previously wrote it into the artifact. Evidence: the
      entry quoted.
- [x] AC7. `Rscript -e 'devtools::test()'` clean and
      `Rscript -e 'devtools::check()'` clean (0 errors, 0 warnings; NOTEs
      justified).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T4, T5
- AC4 → T4, T5
- AC5 → T6
- AC6 → T3
- AC7 → T3

## Tasks

- [x] T1. Add the guards to both generators, deciding how `breaks` accepts its
      documented `0`/`NULL` disable value alongside `validate_count()`'s
      `>= 1` floor; write the `formals()`-walking test first.
- [x] T2. Rebuild the committed artifacts at defaults and compare (AC2).
- [x] T3. NEWS entry, `devtools::document()`, then `devtools::test()` and
      `devtools::check()` clean.
- [x] T4. In `jmgirard/hitop-builder` on a branch: add the four controls,
      binding every value rather than interpolating it, each passed only into
      its own format's call.
- [x] T5. Serve the page locally; verify AC3 and AC4 by downloading and
      parsing each file.
- [x] T6. README section (AC5); open the hitop-side PR; push the builder
      commit at merge.

## Work log

- 2026-08-23: created by /milestone-plan.
- 2026-08-23: criteria audit ran in FULL mode (user-facing tier), inline rather than in a fresh-context subagent (session configured not to spawn agents). One finding: AC1 as first drafted promised rejection of values the target system rejects, a reference rule with no citable source on the shelf and so barred by IP2 — narrowed here to wrong-type rejection, with the format rules moved to a candidate row. No other finding.
- 2026-08-23: the drafted criteria tripped the >7 acceptance-criteria advisory at 8; the REDCap `required` criterion was merged into the criterion covering the other three controls, which the same verification session exercises. Now 7.
- 2026-08-23: plan gate chose package-side guards over validating in the app alone; the latter lost because an R caller would keep the silent-bad-artifact path and the rules would live in a repo cairn does not track. Falsified by a guard rejecting a value the target system actually accepts.
- 2026-08-23: plan gate chose enumerating AC1's domain with `formals()` over a hand-list of the six arguments; the hand-list lost under the bounded-promise rule, since a formal added later would ship unswept. Falsified by a formal the walk cannot reach.
- 2026-08-24: implementation gate settled three open choices: guards live in the shared `build_qualtrics_txt()`/`build_redcap_zip()` builders rather than the two HiTOP-SR wrappers, so all eight generators are swept; `validate_count()` grew `min`/`allow_null` (defaults unchanged) rather than a second helper; a cleared naming box on the builder page falls back to the package default rather than sending a blank through.
- 2026-08-24: AC3 amended at a mini gate, one clause: `Required Field?` on an unticked-required REDCap item row was promised empty and reads `n` (`ifelse(required, "y", "n")` in `build_redcap_zip()`, confirmed by generating one). Criteria audit on the amended wording ran inline in FULL mode (user-facing tier), not in a fresh-context subagent (session configured not to spawn agents); no findings.
- 2026-08-24: T1 done. Guards added to both builders with `call` defaulting to the calling wrapper; `tests/testthat/test-export-arg-guards.R` walks `formals()` of the two HiTOP-SR generators. Discrimination checked: the file fails 8 expectations against the pre-change `R/` and passes 30 with it; full suite 13824 pass, 0 fail.
- 2026-08-24: T2 done. Rebuilt at defaults, `hitopsr_qualtrics.txt` md5 151f508795f5208d0e54334468850177 matches the committed copy; the REDCap archive's `instrument.csv` (407 lines, md5 a82f3ebbadc1e3c97cd081b1a8dd32d6) matches the committed archive's member byte for byte.
- 2026-08-24: Scope amended at a mini gate to add `generate_redcap_hitophsum()`, which builds its dictionary without the shared builders and so did not inherit the guards; it takes `form_name` and `required` directly. Found while checking the NEWS entry's "every generator" claim against the family.

- 2026-08-24: T3 done. NEWS entry added; `devtools::document()` produced no diff; `devtools::test()` 13897 pass / 0 fail / 1 skip; `devtools::check()` Status OK, 0 errors, 0 warnings, 0 notes.
- 2026-08-24: T4 done. `jmgirard/hitop-builder` branch `m050-online-export-controls`, commit eb68d2c: a *Qualtrics and REDCap naming* fieldset with block name, ID prefix, form name and a required checkbox. The three free-text values are bound with `webR.objs.globalEnv.bind()` and referenced by name in the call string; each is passed only into its own format's call. Boxes prefill from `formals()` of the installed package, read at load, so no default is copied into the page.
- 2026-08-24: T5 done, page served from `.claude/launch.json`'s `hitop-builder` config. Two-scale module (Agoraphobia, Appetite Loss; 8 items) with block name `Wave 2 Screening`, ID prefix `W2SCR`, form name `wave2_screening`: the Qualtrics file carried `[[Block:Wave 2 Screening]]` and IDs `W2SCR_066`..`W2SCR_389`; the REDCap `instrument.csv` carried `Form Name` = `wave2_screening` on all 9 rows, `Required Field?` = `n` on all 8 item rows unticked and `y` on all 8 ticked. At defaults both files were byte-identical (sha256 c1a11153.. and 37272d11..) to the same module built on the deployed page. AC4: block name `He said "\\ok\") and left` round-tripped verbatim into the block line, no error, no truncation. An emptied box fell back to the package default with a log line.
- 2026-08-24: T6 done. Builder README gained a "Naming the Qualtrics and REDCap exports" section naming what each control sets and stating that a name the target system refuses surfaces at import, not at generation.

## Decisions

## Review

Fresh evidence gathered 2026-08-24 on branch `m050-online-export-controls`
(PR #56), with the package loaded from the branch and the builder page served
from `.claude/launch.json`'s `hitop-builder` config against the local
`m050-online-export-controls` branch of `jmgirard/hitop-builder`.

- **AC1 — pass.** `tests/testthat/test-export-arg-guards.R` derives its domain
  as `setdiff(names(formals(fn)), c("file", "module", "subset"))` and loops
  that domain for both generators, asserting a `rlang_error` whose message
  names the argument; a guard against the domain emptying silently
  (`expect_true(length(args) > 0)`) and a defaults-still-work control sit
  beside it. A fifth block walks all eleven exported `generate_qualtrics_*()` /
  `generate_redcap_*()` functions over the six shared arguments.
  `devtools::test(filter = "export-arg-guards")`: 103 pass, 0 fail, 0 warn.
- **AC2 — pass.** Rebuilt at defaults from the branch:
  `hitopsr_qualtrics.txt` md5 `151f5087…50177`, identical to the committed
  `inst/extdata/` copy. The rebuilt REDCap archive's only member,
  `instrument.csv` (65,854 bytes, 407 lines, md5 `a82f3eba…d32d6`), is
  byte-identical to the committed archive's member.
- **AC3 — pass.** Local page, webR `hitop 0.2.0`, two-scale module (Cleaning,
  Conversion Symptoms; 13 items). Non-default run — block name
  `Wave 2 Screening`, ID prefix `W2SCR`, form name `wave2_screening`: the
  Qualtrics `.txt` carried `[[Block:Wave 2 Screening]]` and question IDs
  `W2SCR_017` … `W2SCR_400` (14 `[[ID:` lines: 13 items plus
  `start_instructions`); the REDCap `instrument.csv` carried
  `Form Name` = `wave2_screening` on all 14 data rows, and `Required Field?`
  = `y` on all 13 item rows with *required* ticked and `n` on all 13 with it
  unticked (the one `descriptive` instructions row is blank in both, as
  before). Default run — the same module with every control at its prefilled
  default: the Qualtrics `.txt` is byte-identical to the deployed page's
  (3,081 bytes, sha256 `5c6c3289…78a9ee` on both), and the REDCap
  `instrument.csv` identical (sha256 `00aa3fe1…9aa8f3` on both; the `.zip`
  differs only in its stored timestamp).
- **AC4 — pass.** Block name typed into the page as `He said "\ok") and left`
  (character codes confirmed: `"` = 34, `\` = 92, `")` = 34, 41). The
  Qualtrics download's block line reads `[[Block:He said "\ok") and left]]`,
  verbatim and untruncated; the file carried its full 14 `[[ID:` lines, the
  page log reported a normal 3,096-byte write, and no error was reported.
- **AC5 — pass.** `README.md` in the builder repo gains
  *Naming the Qualtrics and REDCap exports*: it names what each of the four
  controls sets (the `[[Block:…]]` line, the `[[ID:…]]` prefix, the
  `Form Name` column, the `Required Field?` column), and states that "a name
  either one refuses is refused when you import the file, not when you build
  it here." It carries the verification date: "Verified 2026-08-24 on a
  two-scale module."
- **AC6 — pass.** NEWS.md records: "**The Qualtrics and REDCap generators now
  check their arguments** (breaking). … previously wrote whatever they were
  handed into the import file: `id_prefix = 1` wrote question IDs reading
  `1_001` … Each now raises an error naming the argument, and no file is
  written. `breaks` still accepts `0` and `NULL` to turn pagination off.
  Files built from valid arguments are byte-for-byte unchanged."
- **AC7 — pass.** `devtools::test()`: 13,897 pass, 0 fail, 1 skip.
  `devtools::check()`: Status OK — 0 errors, 0 warnings, 0 notes (3m 24s).

### Consistency gate

- `cairn_validate.py`: exit 0, all 16 PASS checks green; 20 advisory warnings,
  all pre-existing dangling `D-00x` tokens from the pre-migration decisions.
- No `DESIGN.md` principle changed, so `cairn_impact.py` does not apply.
- Toolchain checks (`r-package` profile's `consistency-gate`):
  `devtools::document()` produced no diff (only the milestone file is
  modified); no generated file hand-edited; `README.Rmd`/`README.md` untouched
  by this milestone; `pkgdown::check_pkgdown()` — "No problems found";
  NEWS.md entry present (AC6); no new top-level files, so no `.Rbuildignore`
  entry needed; `devtools::check()` Status OK (0/0/0).

### Independent review

The three lenses ran inline in this session rather than in fresh-context
subagents (session configured not to spawn agents), on the full
`main..HEAD` diff, on `git log`/`blame` of the touched lines, and on the
archived `## Review` sections of the milestones that previously touched
`R/util.R`, `R/generate_qualtrics.R`, and `R/generate_redcap.R` (M022, M024,
M029, M031, M040). The GitHub inline-review surface was probed
(`gh api repos/jmgirard/hitop/pulls/comments`) and is empty, as M91 found
generally — archived Review sections are the evidence.

Findings, most severe first:

1. **[blame-history] `validate_count()`'s `max` gained a default of `Inf`,
   where M031 made it a required argument.** A future caller that forgets an
   upper bound now silently gets an unbounded check;
   `hitop:::validate_count(99, arg = "x")` returns without error, where before
   this diff it aborted with "argument \"max\" is missing". Both new call
   sites want unbounded deliberately (`breaks` has no ceiling), and
   `rank_scales()` still passes its `max`. Disposition: **rejected** — the
   default is what makes the helper reusable for an unbounded count, the
   milestone's stated purpose for touching it, and every call site in `R/` is
   checked. Logged here rather than fixed.

No other finding. Checks against the prior-review record: M024's actioned
finding (a bypassable `isTRUE()` guard letting `include_subscales = 1`
through) is not reintroduced — `validate_flag()` uses `rlang::is_bool()`, and
`required = 1` and `include_instructions = 1` are both refused. M031's
`arg`/`call` convention is followed, with `call` threaded from the builders
so the abort blames the exported wrapper (asserted by a test).

Probes run beyond the acceptance criteria: `required = 1`,
`include_instructions = 1`, `id_prefix = NA`, `block_name = c("a", "b")`,
`breaks = 2.5`, and `generate_redcap_hitophsum(required = 1)` each abort
naming their argument, and no output file exists after a refused call.

## Work log (review)

- 2026-08-24: review ran. All 7 acceptance criteria pass on fresh evidence; consistency gate clean; three lenses inline, one finding, rejected at triage. PR #56 opened as a draft.
