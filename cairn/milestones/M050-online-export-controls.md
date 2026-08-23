# M050: Naming and response controls for the builder's online exports

- **Status:** planned
- **Priority:** normal
- **Depends on:** M049
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** —

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
"disable pagination". Tests, NEWS.

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

- [ ] AC1. Every formal of `generate_qualtrics_hitopsr()` and
      `generate_redcap_hitopsr()` other than `file`, `module`, and `subset`
      rejects a wrong-type value with a `cli` error naming that argument. The
      domain is enumerated by a test that walks `formals()` of the two
      functions and skips only those three by name, so a formal added later is
      swept without editing the test. Evidence: the test source and its
      passing output.
- [ ] AC2. The guards changed no output. Rebuilding at defaults,
      `hitopsr_qualtrics.txt` matches the committed `inst/extdata/` copy by
      md5, and the REDCap archive's `instrument.csv` matches the committed
      archive's member content. Evidence: both comparisons' output.
- [ ] AC3. In a browser loading the modified page, all four controls drive
      their downloads. A two-scale module downloaded with a non-default block
      name, ID prefix, and form name yields a Qualtrics `.txt` whose
      `[[Block:...]]` line and `[[ID:...]]` question IDs carry the supplied
      strings, and a REDCap `instrument.csv` whose `Form Name` column carries
      the supplied form name; with *required* unticked that file's
      `Required Field?` column is empty on every item row and with it ticked
      reads `y` on every item row, row count stated. With every control left
      at its default, the same module's Qualtrics `.txt` is byte-identical,
      and its REDCap `instrument.csv` identical, to what the currently
      deployed page produces. Evidence: the strings and column values quoted
      from each download, and the default-state comparison.
- [ ] AC4. Free text reaches R only through `bind()`, never interpolation: a
      block name containing a double quote, a backslash, and the sequence `")`
      produces a Qualtrics download whose block line carries that string
      verbatim, with no error reported on the page and no truncation.
      Evidence: the input string and the block line from the download.
- [ ] AC5. The builder `README.md` gains a section naming what each of the
      four controls sets in its target system, and stating that a name the
      target system rejects surfaces at import time rather than at generation.
      Evidence: the section quoted, carrying the verification date.
- [ ] AC6. NEWS.md records that these arguments now error on a wrong-type
      value where they previously wrote it into the artifact. Evidence: the
      entry quoted.
- [ ] AC7. `Rscript -e 'devtools::test()'` clean and
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

- [ ] T1. Add the guards to both generators, deciding how `breaks` accepts its
      documented `0`/`NULL` disable value alongside `validate_count()`'s
      `>= 1` floor; write the `formals()`-walking test first.
- [ ] T2. Rebuild the committed artifacts at defaults and compare (AC2).
- [ ] T3. NEWS entry, `devtools::document()`, then `devtools::test()` and
      `devtools::check()` clean.
- [ ] T4. In `jmgirard/hitop-builder` on a branch: add the four controls,
      binding every value rather than interpolating it, each passed only into
      its own format's call.
- [ ] T5. Serve the page locally; verify AC3 and AC4 by downloading and
      parsing each file.
- [ ] T6. README section (AC5); open the hitop-side PR; push the builder
      commit at merge.

## Work log

- 2026-08-23: created by /milestone-plan.
- 2026-08-23: criteria audit ran in FULL mode (user-facing tier), inline rather than in a fresh-context subagent (session configured not to spawn agents). One finding: AC1 as first drafted promised rejection of values the target system rejects, a reference rule with no citable source on the shelf and so barred by IP2 — narrowed here to wrong-type rejection, with the format rules moved to a candidate row. No other finding.
- 2026-08-23: the drafted criteria tripped the >7 acceptance-criteria advisory at 8; the REDCap `required` criterion was merged into the criterion covering the other three controls, which the same verification session exercises. Now 7.
- 2026-08-23: plan gate chose package-side guards over validating in the app alone; the latter lost because an R caller would keep the silent-bad-artifact path and the rules would live in a repo cairn does not track. Falsified by a guard rejecting a value the target system actually accepts.
- 2026-08-23: plan gate chose enumerating AC1's domain with `formals()` over a hand-list of the six arguments; the hand-list lost under the bounded-promise rule, since a formal added later would ship unswept. Falsified by a formal the walk cannot reach.

## Decisions

## Review
