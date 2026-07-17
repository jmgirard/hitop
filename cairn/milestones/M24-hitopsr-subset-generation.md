<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M24: HiTOP-SR scale-subset generation (subset descriptor + docx/Qualtrics/REDCap)

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** IP1, GP3, GP4
- **Branch/PR:** —

## Goal

Let researchers select a subset of HiTOP-SR scales and generate DOCX/Qualtrics/REDCap instruments containing only those items, preserving original HSR item numbering.

## Scope

**In:**

- A new exported subset-descriptor constructor `hitop_subset(instrument = "hitopsr", scales = ...)` returning a validated `hitop_subset` object that resolves chosen scale names (`hitopsr_scales$camelCase`) → item numbers (union of the scales' `itemNumbers`, sorted ascending, **original HSR numbering preserved**), reverse flags, and the reduced scale→item map. Signature is forward-shaped for later instruments but only `"hitopsr"` is implemented (others error "not yet supported").
- Scale-name validation with actionable `cli` errors (unknown scale names named individually; empty selection rejected).
- The three SR generators (`generate_docx_hitopsr`, `generate_qualtrics_hitopsr`, `generate_redcap_hitopsr`) gain a `subset = NULL` argument; when a `hitop_subset` is supplied they emit an artifact containing only the subset's items — verbatim item text, reverse indicators, and per-scale scoring tables restricted to the subset, all with original HSR numbers.
- Docs (roxygen + one worked example), a NEWS entry, and the `_pkgdown.yml` reference listing for `hitop_subset`.

**Out:**

- Scoring data collected from a subset instrument → candidate "Score HiTOP-SR subset-collected data" (depends on M24).
- BR and PID-5 subsetting → candidate "Generalize modularization to BR/PID-5" (BR scales overlap — p-Factor spans all items — so it needs a different design).
- Renumbering subset items 1..k → not done; original HSR numbers are preserved so the existing scoring key still maps (design decision of this milestone).
- Subscale-level (`hitopsr_subscales`) subsetting → out; main 76 scales only this milestone.
- No change to `hitopsr_items`/`hitopsr_scales` keying content (filter-only; no sign-off needed).

## Acceptance criteria

- [ ] `hitop_subset("hitopsr", scales = c(...))` returns an object whose resolved item numbers equal the union of the chosen scales' `hitopsr_scales$itemNumbers`, sorted ascending in original HSR numbering — verified against an independent hand-derived expected set for ≥2 distinct scale selections.
- [ ] `hitop_subset()` errors on unknown scale name(s), naming each offending name via `cli`; errors on an empty/zero-scale selection. (Each error branch fired.)
- [ ] `generate_docx_hitopsr(subset = s)` emits a DOCX whose parsed items are exactly the subset's items (verbatim text + reverse markers) and whose scoring table lists only the subset scales with original HSR item numbers (parse-and-compare per D-010).
- [ ] `generate_qualtrics_hitopsr(subset = s)` emits a Qualtrics `.txt` whose parsed items and IDs are exactly the subset's items with original HSR numbering.
- [ ] `generate_redcap_hitopsr(subset = s)` emits a REDCap zip whose parsed fields are exactly the subset's items with original HSR numbering.
- [ ] Default calls (`subset = NULL`) produce parse-identical output to the current full-instrument generators — an explicit no-regression assertion for all three formats, plus the pre-existing generator tests still passing.
- [ ] `devtools::document()` clean (no diff); profile `verify` clean (`devtools::test()` pass, `devtools::check()` 0/0/0); NEWS + `_pkgdown.yml` updated.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [ ] T1: Implement `hitop_subset()` + a `hitop_subset` S3 object and scale-name validation in a new `R/subset.R` (tests-first: resolution against a hand-derived oracle for ≥2 selections; unknown-name and empty-selection error branches). *(RB tripwire: irreversible-api — the exported constructor signature/shape)*
- [ ] T2: Add an internal base-R helper that reduces an `*_items` table + scale map to a subset, preserving original numbering; keep it instrument-general for later reuse.
- [ ] T3: Wire `subset` into `generate_docx_hitopsr`; add a parse-and-compare test (D-010 style) for a subset artifact.
- [ ] T4: Wire `subset` into `generate_qualtrics_hitopsr`; add a parse-and-compare test.
- [ ] T5: Wire `subset` into `generate_redcap_hitopsr`; add a parse-and-compare test.
- [ ] T6: Add full-vs-`subset=NULL` equivalence tests (no-regression) for all three generators.
- [ ] T7: Roxygen docs + worked example, NEWS entry, `_pkgdown.yml` reference; run `document()`, `test()`, `check()`.

## Work log

- 2026-07-17: created by /milestone-plan. Forks decided at the gate: SR only · subset-descriptor object (not a per-function `scales=` arg, which would reverse the deliberate "no scales arg" convention — D-006/D-012) · preserve original HSR numbering · generate-first (scoring deferred to a dependent candidate).

## Decisions

## Review
