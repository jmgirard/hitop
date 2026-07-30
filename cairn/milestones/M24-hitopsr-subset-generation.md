<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M24: HiTOP-SR scale-subset generation (subset descriptor + docx/Qualtrics/REDCap)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** IP1, GP3, GP4
- **Branch/PR:** `m24-hitopsr-subset-generation`

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

- [x] T1: Implement `hitop_subset()` + a `hitop_subset` S3 object and scale-name validation in a new `R/subset.R` (tests-first: resolution against a hand-derived oracle for ≥2 selections; unknown-name and empty-selection error branches). *(RB tripwire: irreversible-api — the exported constructor signature/shape)*
- [x] T2: Add an internal base-R helper that reduces an `*_items` table + scale map to a subset, preserving original numbering; keep it instrument-general for later reuse.
- [x] T3: Wire `subset` into `generate_docx_hitopsr`; add a parse-and-compare test (D-010 style) for a subset artifact.
- [x] T4: Wire `subset` into `generate_qualtrics_hitopsr`; add a parse-and-compare test.
- [x] T5: Wire `subset` into `generate_redcap_hitopsr`; add a parse-and-compare test.
- [x] T6: Add full-vs-`subset=NULL` equivalence tests (no-regression) for all three generators.
- [x] T7: Roxygen docs + worked example, NEWS entry, `_pkgdown.yml` reference; run `document()`, `test()`, `check()`.

## Work log

- 2026-07-30: /milestone-implement started on branch `m24-hitopsr-subset-generation`.
- 2026-07-30: implement gate settled three open forks (scale-name vocabulary, Qualtrics ID padding, `include_subscales` collision) — see the Decisions entry below.
- 2026-07-30: T7 done — `hitop_subset` listed under Item Export in `_pkgdown.yml`, NEWS entries for the subset feature and the padding change, worked `@examples` on all four functions. `document()` no-diff, `pkgdown::check_pkgdown()` clean, `devtools::test()` FAIL 0 | PASS 9776, `devtools::check()` 0 errors | 0 warnings | 0 notes. Status -> review.
- 2026-07-30: gotcha — `R/generate_qualtrics.R` and `R/generate_redcap.R` are stored with CRLF line endings; a whole-file rewrite silently converted them to LF and inflated the branch diff by ~1,700 lines. Restored in a dedicated commit; candidate LESSONS line at review.
- 2026-07-30: T3-T6 done — `subset = NULL` added to all three SR generators; Qualtrics ID padding now derives from the largest item number (was row count); `include_subscales` + `subset` errors; 3 new test blocks per format plus `test-subset-generation.R` (subset = NULL and an all-76-scales subset both reproduce the full artifact). `devtools::test()` FAIL 0 | PASS 9776.
- 2026-07-30: moved `skip_if_no_zip()`/`skip_if_no_docx()` from two test files into `helper-generators.R` so the new subset tests share them (no behavior change).
- 2026-07-30: T1+T2 done — `R/subset.R` adds exported `hitop_subset()`, a `print()` method, and the internal `apply_subset()` reducer; `test-subset.R` adds 29 assertions incl. hand-derived item oracles for 3 selections. `devtools::test()` FAIL 0 | PASS 9694.
- 2026-07-17: created by /milestone-plan. Forks decided at the gate: SR only · subset-descriptor object (not a per-function `scales=` arg, which would reverse the deliberate "no scales arg" convention — D-006/D-012) · preserve original HSR numbering · generate-first (scoring deferred to a dependent candidate).

## Decisions

### 2026-07-30 (T1): `hitop_subset()` API shape (the plan's `irreversible-api` tripwire)

Settled at the implement question gate rather than escalated, the maintainer choosing among three stated options per fork.

1. **Scale-name vocabulary — accept both forms, case-insensitively.** `scales` matches against `hitopsr_scales$Scale` (display) *or* `$camelCase` (the scored-output stem), ignoring case; the object canonicalizes to display names plus stems. Verified safe: lowercasing both columns yields no key that maps to two different scales, so the union lookup is unambiguous. Rejected: camelCase-only (one vocabulary, but errors on the names printed on the instrument the user is holding) and display-only (diverges from the scoring vocabulary).
2. **Qualtrics ID padding — fixed in the shared helper.** `build_qualtrics_txt()` computed its zero-pad width from `nrow(items)`, which under a subset under-pads (item 7 → `HSR_07` beside item 312 → `HSR_312`). Width now derives from the largest item number, which is identical for all six existing generators and is asserted so. Rejected: passing an explicit width from the SR generator only, which leaves the latent bug for the next instrument that filters items.
3. **`include_subscales` + `subset` — error.** A subscale can draw items from outside the chosen scales, so the combination is rejected with a `cli` error naming the conflict. Rejected: including only fully-contained subscales (invents a containment rule the plan scoped Out) and silently ignoring the argument (GP1: deviations are loud).


## Review
