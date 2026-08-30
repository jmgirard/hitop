# M068: The HiTOP-BR carries score intervals against the paper's development sample

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3
- **Branch/PR:** `m068-hitopbr-score-intervals`

## Goal

`interval_hitopbr()` converts scored HiTOP-BR columns into a regression-based
true-score estimate and confidence interval against the eight "Superspectra and
Spectra Scales" rows of simms2026 Table 1, shipped as `hitopbr_devstats`.

## Scope

Surface tier: **user-facing** — a new exported function, a new exported dataset,
and published help pages and vignette prose that readers act on.

**In:** Transcribe Table 1's eight Superspectra and Spectra rows (`# Items`,
alpha, `M`, `SD`, `Range`) in `data-raw/hitopbr_table1.R`, with an independent
coordinate-extraction verifier mirroring `data-raw/verify_hitopsr_devstats.R`.
Reconcile the printed `# Items` against `hitopbr_items` for every row that
disagrees, at Jeff's sign-off (IP1 keying content). Ship `hitopbr_devstats` and
`interval_hitopbr()` as a thin `interval_engine()` wrapper. Widen the source
admission to cover this block, and register the new oracles.

**Out:** Deprecating `calc_se` → M069, which depends on this. A PID-5 interval
surface → the standing ROADMAP candidate (blocked on an ingested reliability).
Recomputing Table 1 from `Prolific data HiTOP-SR.sav` → the standing
recomputation candidate row. Renaming `hitopsr_devstats$scale` to `Scale` → the
standing column-shape candidate row; this table matches its sibling as built.

## Acceptance criteria

- [ ] AC1 `interval_hitopbr(data, scores, srange, prefix, level, append)` is
      exported, documented, and carries a `_pkgdown.yml` reference row; for each
      of the eight scales `hitopbr_devstats` carries it returns the estimate and
      bounds of Schmukle (2026) Eqs (10)–(12), p. 821. Verified by a closed-form
      oracle in the O-001 form: the reference mean, SD and reliability written
      into the test as printed in Table 1, **not** read from `hitopbr_devstats`,
      each scale probed at a score below, at, and above its reference mean and
      at levels 0.95 and 0.80.
- [ ] AC2 `hitopbr_devstats` ships one row per HiTOP-BR scale with
      `hitopsr_devstats`'s column contract, and every numeric cell equals its
      Table 1 printed cell — checked cell by cell by a new
      `data-raw/verify_hitopbr_devstats.R`, an independent coordinate extraction
      of the same PDF pages, which also checks Table 1's `Range` cells against
      the `c(1, 4)` coding `interval_hitopbr()` hardcodes (IP2), and whose header
      records a planted-defect demonstration varying defect form as well as
      location, in the form `data-raw/verify_hitopsr_devstats.R:32-40` records.
- [ ] AC3 A CI-runnable invariant in `tests/` asserts, for every row of
      `hitopbr_devstats`, that its `nItems` equals `lengths(hitopbr_scales$itemNumbers)`
      for the same stem, carrying the reconciled disposition for any row AC5
      records as disputed; and `setdiff(sub("^hbr_", "", names(score_hitopbr(sim_hitopbr,
      items = 1:45, append = FALSE))), hitopbr_devstats$camelCase)` is empty.
- [ ] AC4 `interval_hitopbr()` reports the two conditions `interval_hitopsr()`
      does, naming the BR dataset: a score column with no `hitopbr_devstats` row
      returns `NA` in all three interval columns under a warning of class
      `hitop_interval_uncovered`, and a call whose `srange` is not `c(1, 4)`
      returns `NA` in every interval column under `hitop_interval_coding`.
- [ ] AC5 For every row where Table 1's printed `# Items` differs from the count
      `hitopbr_items` yields — the set `data-raw/verify_hitopbr_devstats.R`
      reports, not a pre-counted list — the `hitopbr_devstats` help page states
      both counts and the disposition Jeff signed off on, and
      `cairn/references/simms2026.md` and `cairn/SOURCES.md` carry the same
      disposition as a resolved or standing OQ (IP1).
- [ ] AC6 The development-sample caveat D-032 requires appears wherever these
      numbers surface — the `interval_hitopbr()` and `hitopbr_devstats` help
      pages, `vignettes/hitopbr_scoring.Rmd`, and NEWS.md — each saying the
      reference group is a development sample and not a community norm; asserted
      by a prose guard over the generated Rd and the vignette, in the form
      `tests/testthat/test-help-se-prose.R` uses.
- [ ] AC7 The checks `cairn/PROFILE.md`'s `## consistency-gate` slot lists are
      clean, including `devtools::document()` with no diff, `devtools::test()`
      FAIL 0, `devtools::check()` 0 errors and 0 warnings, and
      `pkgdown::check_pkgdown()`.

## Coverage

- AC1 → T5, T6
- AC2 → T1, T2, T4
- AC3 → T3, T4, T6
- AC4 → T5, T6
- AC5 → T3, T4
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1 Read Table 1's Superspectra and Spectra block (shelf pp. 49–51) and
      transcribe the eight rows into `data-raw/hitopbr_table1.R`, mirroring
      `data-raw/hitopsr_table1.R`'s structure and its SHA-256 shelf pin.
- [x] T2 Write `data-raw/verify_hitopbr_devstats.R` as an independent coordinate
      extraction of the same pages (the existing extractor already returns
      `rangeLo`/`rangeHi` for `block == "superspectra"`); prove it able to fail
      one planted defect at a time, varying form, with clean controls, and record
      that run in its header.
- [x] T3 Tabulate each BR scale's item count from `hitopbr_items` against Table
      1's printed `# Items`; bring every disagreement to Jeff for sign-off; write
      the agreed disposition into `cairn/references/simms2026.md`,
      `cairn/SOURCES.md`, and the `hitopbr_devstats` help page.
- [x] T4 Build and ship `hitopbr_devstats` (data-raw script, `R/data.R` doc,
      `_pkgdown.yml` row), matching `hitopsr_devstats`'s eight columns.
- [x] T5 Add `R/interval_hitopbr.R` as a thin `interval_engine()` wrapper, with
      roxygen mirroring `R/interval_hitopsr.R`'s Details and its two reports.
- [x] T6 Tests: the AC1 closed-form oracle, the AC3 invariants, both condition
      classes, and the argument-surface cases `test-interval_hitopsr.R` covers;
      register the new oracles as rows in `cairn/ORACLES.md`.
- [x] T7 NEWS.md entry and the AC6 prose guards.
- [x] T8 Append the D-entry widening D-032/D-042's source admission from the
      HiTOP-SR rows of Table 1 to its Superspectra and Spectra block; run the
      full consistency gate.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in FULL mode (user-facing tier) in a fresh-context [O] reader Jeff authorized at the gate; it returned 10 findings on this milestone's criteria — source admission unentried, oracle constants self-referential, probe axes unvaried, verifier promise instrument-bound with no CI-runnable half and no falsifiability requirement, nItems contradiction, pre-counted disagreement domain, disposition unbound to a shipped surface, the c(1,4) constant unchecked against IP2, D-032's caveat bound only to NEWS, and a hand-listed consistency gate — every one fixed in the criteria above before committing.
- 2026-08-30: plan gate chose reconciling the disputed `# Items` against `hitopbr_items` at Jeff's sign-off over recording the divergence unreconciled (M041's posture for Table 1's contradicted .61 alpha), because leaving it standing blocks the CI-runnable invariant that is the only in-suite guard on a transcribed table; falsified by the accepted paper printing counts that agree with neither side.
- 2026-08-30: plan chose `hitopbr_devstats` matching `hitopsr_devstats`'s columns verbatim — including the lowercase `scale` the standing column-shape candidate row flags — over naming it `Scale` to match the four keying tables, because the two devstats tables feed one shared engine and the candidate row exists to rename them together; falsified by a user joining a devstats table to a keying table and hitting the mismatch before that rename lands.
- 2026-08-30: question gate. Jeff signed off on correcting `hitopbr_items` row 36 (`HiTOP_69`, "I had a hard time asserting myself to others.") from `Detachment` to `Internalizing` inside this milestone, after asking whether the paper could be the wrong side; four statements say Internalizing and none but the package's own CSV says Detachment — the development workbook's `item-to-scale` sheet, the same workbook's `scoring syntax` (`BInternalizing = MEAN.7(HiTOP_69, ...)`, `BDetachment = MEAN.4(...)` over five items), Table 4's promax loadings (.67 on INT, blank on DET), and the item's HiTOP-SR home (Submissiveness). The CSV has carried `Detachment` since `64b36178` (2025-06-08) with no recorded source.
- 2026-08-30: question gate chose `type = "scale"` for all eight `hitopbr_devstats` rows over `superspectrum`/`spectrum`, because Table 1 separates Externalizing and p-factor from the other six by a blank row and no printed label, and reading a shipped content column out of a typographic gap is inference (IP2).
- 2026-08-30: T3 grew the keying correction the gate approved and its ripple — rebuilt `hitopbr_items`/`hitopbr_scales`, the two HiTOP-BR Word forms whose scoring-key page prints the item lists, and a breaking-change NEWS entry; tasks reordered to run T3 and T4 before T2, since the verifier diffs a shipped table that must exist first.
- 2026-08-30: T1 done. `data-raw/hitopbr_table1.R` transcribes Table 1's eight Superspectra and Spectra rows read at 200 dpi (the two disputed `# Items` cells re-read from a 400 dpi crop), pins the shelf sha256 under its own name, and sources no extractor.
- 2026-08-30: T3 done. `hitopbr_items` row 36 corrected to `Internalizing`; `hitopbr_items`/`hitopbr_scales` rebuilt, the two HiTOP-BR Word forms and their staged copies rebuilt (their scoring-key page now prints Detachment 7, 12, 30, 31, 37 and Internalizing 8, 9, 18, 22, 23, 36, 42, 44), the manifest re-stamped, a breaking-change NEWS entry added, and the disposition recorded as resolved in `cairn/references/simms2026.md` and as a new `cairn/SOURCES.md` section. The existing hardcoded Detachment alpha oracle in `test-reliability.R` went red on the change and was corrected, with an Internalizing counterpart added; a new membership oracle in `test-score_hitopbr.R` states the workbook's six primary sections by the source's own item identifiers, shown red by putting item 36 back and green on the restored control.
- 2026-08-30: T4 done. `data-raw/hitopbr_devstats.R` builds `hitopbr_devstats` from the transcription with a one-entry name map (`p-factor` -> `p-Factor`), an empty declared exception set re-derived at every rebuild, and stopifnot guards on row count, residue, item counts, reliability range and coding range; help page in `R/data.R` and two `_pkgdown.yml` rows added.
- 2026-08-30: T2 done. `data-raw/verify_hitopbr_devstats.R` runs five comparisons — shelf pins, Table 1 against the transcription (48 cells), Table 1 against the shipped object (32 cells), printed `# Items` against the keying tables (0 disagreeing), and the Range column against the `c(1, 4)` coding. Proven able to fail on eight plants varying form and location across all five comparisons, each named and exiting 1, with clean controls before and after; the run is recorded in the script header.
- 2026-08-30: T5 done. `R/interval_hitopbr.R` wraps `interval_engine()`; its Details mirror `interval_hitopsr()`'s and add the two BR-specific facts — every scale returns a lower bound below the response floor at a floor score, and Externalizing and p-Factor draw on the same items as the six spectra. The engine's stale "only interval_hitopsr() calls this" comment corrected.
- 2026-08-30: T6 done. `tests/testthat/test-interval_hitopbr.R` carries the closed-form oracle over all eight scales at three probe scores and two levels, the two CI-runnable invariants, both condition classes asserted by class and by the dataset each report names, and the argument surface `test-interval_hitopsr.R` covers. Three oracle rows added to `cairn/ORACLES.md`.
- 2026-08-30: T7 done. NEWS gains a development-version heading with the new-feature entries and the breaking change; `vignettes/hitopbr_scoring.Rmd` gains a Confidence Intervals section with the reference-group caveat; `tests/testthat/test-interval-br-prose.R` asserts the caveat and the N on all four surfaces, shown red by removing the phrase from the vignette and by a terminator matching nothing.
- 2026-08-30: T8 consistency gate. Two existing exhaustive export sweeps went red on the new function and were extended to it: `test-append-collision.R`'s probe table (with a HiTOP-BR score frame as its input) and `test-error-prose.R`'s terminator table and selection list. `cairn_validate` all checks pass with 23 pre-existing advisories; `cairn_impact` reports no changed principles; `pkgdown::check_pkgdown()` finds no problems.
- 2026-08-30: T8 done. D-048 appended; `hitopbr_devstats` added to the `utils::globalVariables()` declaration, which cleared the one NOTE `R CMD check` raised for it. Gate results on the final tree: `devtools::document()` no diff, `devtools::test()` FAIL 0 WARN 0 SKIP 4 PASS 16071, `devtools::check()` 0 errors / 0 warnings / 0 notes (15m 22s), `pkgdown::check_pkgdown()` no problems, `cairn_validate` all checks pass.

## Decisions

## Review
