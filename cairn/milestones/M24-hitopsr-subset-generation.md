<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M24: HiTOP-SR scale-subset generation (subset descriptor + docx/Qualtrics/REDCap)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** IP1, GP3, GP4
- **Branch/PR:** `m24-hitopsr-subset-generation` / https://github.com/jmgirard/hitop/pull/27

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

- [x] `hitop_subset("hitopsr", scales = c(...))` returns an object whose resolved item numbers equal the union of the chosen scales' `hitopsr_scales$itemNumbers`, sorted ascending in original HSR numbering — verified against an independent hand-derived expected set for ≥2 distinct scale selections.
- [x] `hitop_subset()` errors on unknown scale name(s), naming each offending name via `cli`; errors on an empty/zero-scale selection. (Each error branch fired.)
- [x] `generate_docx_hitopsr(subset = s)` emits a DOCX whose parsed items are exactly the subset's items (verbatim text + reverse markers) and whose scoring table lists only the subset scales with original HSR item numbers (parse-and-compare per D-010).
- [x] `generate_qualtrics_hitopsr(subset = s)` emits a Qualtrics `.txt` whose parsed items and IDs are exactly the subset's items with original HSR numbering.
- [x] `generate_redcap_hitopsr(subset = s)` emits a REDCap zip whose parsed fields are exactly the subset's items with original HSR numbering.
- [x] Default calls (`subset = NULL`) produce parse-identical output to the current full-instrument generators — an explicit no-regression assertion for all three formats, plus the pre-existing generator tests still passing.
- [x] `devtools::document()` clean (no diff); profile `verify` clean (`devtools::test()` pass, `devtools::check()` 0/0/0); NEWS + `_pkgdown.yml` updated.

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

Fresh evidence gathered 2026-07-30 on `m24-hitopsr-subset-generation` @ PR #27.
All commands run at review time; no result carried over from implementation.

### Acceptance-criterion evidence

- **AC1 (resolution oracle).** `test-subset.R` blocks 1–3 pass (part of 9 blocks / 27 assertions, 0 fail). Three selections checked against item numbers hand-derived from `hitopsr_items$Scale`: Agoraphobia+Appetite Loss → 66,109,118,144,202,260,291,389; Antisocial Behavior+Romantic Disinterest → 13 items incl. 42 and 390; Romantic Disinterest alone → 42,152,187,310,338. All ascending, original HSR numbering. `test-subset-generation.R` adds the all-76-scales case → exactly `hitopsr_items$HSR`, 405 items.
- **AC2 (error branches).** `test-subset.R` fires each branch: unknown names (both named in one message), `character(0)`, `NULL`, non-character, `NA_character_`, unsupported instrument (`hitopbr`, `pid5`), unknown instrument. 0 fail.
- **AC3 (DOCX parse-and-compare).** `test-generate_docx` 9 blocks / 73 assertions, 0 fail. Subset DOCX: all 8 kept items present as `<HSR>.  <Text>`, 25 sampled non-subset texts absent, no renumbering (`"1.  "` form absent); scoring table shows `66, 109, 118, 260, 291` and `42, 152, 187, 310(R), 338` — HSR 310 keeps its reverse marker — and omits out-of-subset scales.
- **AC4 (Qualtrics parse-and-compare).** `test-generate_qualtrics` 7 blocks / 56 assertions, 0 fail. Parsed `num` and `text` equal the filtered `hitopsr_items` exactly; IDs `HSR_066`…`HSR_389`, all the same width.
- **AC5 (REDCap parse-and-compare).** `test-generate_redcap` 11 blocks / 63 assertions, 0 fail. Data-dictionary radio rows equal the 8 kept items; field names `hsr_066`…`hsr_389`; the descriptive instructions row survives.
- **AC6 (no-regression).** `test-subset-generation.R` 4 blocks / 14 assertions, 0 fail. Qualtrics: md5 identical across default / `subset = NULL` / all-76-scales. REDCap: parsed data dictionaries identical across the three. DOCX: `word/document.xml` identical across the three. Padding lock: all five Qualtrics generators keep their pre-change width (3,2,3,3,2) and contiguous 1..n numbering.
- **AC7 (toolchain).** `devtools::document()` no diff · `devtools::test()` FAIL 0 | WARN 0 | SKIP 1 | PASS 9776 (main: 9694) · `devtools::check()` 0 errors / 0 warnings / 0 notes · `pkgdown::check_pkgdown()` no problems · NEWS.md carries both entries · `_pkgdown.yml` lists `hitop_subset` under Item Export.

### Consistency gate

- `cairn_validate.py` exit 0 — all 16 checks PASS. Two pre-existing advisories, neither from this milestone: a scaffold deprecation (`cairn/references/pdf/` superseded by `cairn/references/sources/`) and 26 dangling id tokens (the legacy D-001–D-012 in DESIGN.md and legacy M-ids).
- Profile `consistency-gate` slot: `document()` no-diff ✓ · no hand-edited generated files ✓ · README untouched by this branch ✓ · `check_pkgdown()` ✓ · NEWS entry ✓ · no new top-level files needing `.Rbuildignore` ✓ · `check()` clean ✓.
- No `DESIGN.md` principle changed, so `cairn_impact` is not run.
- Returns to `in-progress`: none (first review pass; thrash rule not engaged).

### Independent review (3 fresh-context lenses + scorer)

- **[O] diff-bug (Opus)** — 18 candidate findings. **[S] blame-history (Sonnet)** — 1 (the CRLF divergence), no conflict with any recorded decision; it verified the padding change was not a deliberate historical choice, that moving the skip guards matches M10's intent for `helper-generators.R`, and that the `subset`-object design honors the D-006/D-012 "no scales arg" convention rather than reversing it. **[S] prior-review record (Sonnet)** — 0 findings; the `gh api .../pulls/comments` probe returned `[]`, so the per-PR thread walk was skipped, and no archived `## Review` finding or LESSONS line bears on these files as a regression.
- **[S] scorer (Sonnet)**, given the diff and the plan, scored all 19 against the rubric. One scored ≥ 80.

**Actioned (score ≥ 80): 1 of 19.**

- **#1, score 85 — `R/generate_docx.R:129`, the `include_subscales` + `subset` guard was bypassable.** The guard used `isTRUE(include_subscales)` while the consumer 27 lines below used plain `if (include_subscales)`, so `include_subscales = 1` slipped the guard and wrote an 8-item form whose scoring table carried 17 "(Subscale)" rows listing items not on the form — exactly the artifact milestone Decision #3 rejects. **Fixed at review**: the guard now tests the same truthiness as the consumer, with a comment saying why, plus a regression test over `1`, `1L`, and a non-coercible string. Reproduced before the fix and confirmed closed after.

**Logged, not actioned (score < 80): 18.** Surfaced here, never silently dropped.

- 78 — DOCX subset test samples 25 of 397 dropped item texts and never asserts the emitted item count (Qualtrics/REDCap equivalents are exact).
- 68 — `hitop_subset("hitopsr")` with `scales` omitted gives a bare R error, not a `cli_assert` message.
- 62 — the returned object omits the per-scale item map Scope In names; `apply_subset()` re-derives it. Belongs with the existing `Score HiTOP-SR subset-collected data` candidate, which is the consumer that would need it.
- 55 — `instrument` precedes the required `scales`, so a positional first call errors about the wrong argument.
- 52 — the milestone Decisions text says "all six existing generators"; five funnel through `build_qualtrics_txt()` (HSUM has no exported Qualtrics generator). The test correctly iterates five.
- 42 — roxygen/NEWS say subset data "can still be scored against the full key"; `score_hitopsr()` requires all 405 columns, so NA-padding is needed first.
- 35 — pad width derives from the subset's largest item number, not the instrument's; unreachable for the SR (minimum per-scale maximum is 151) and only live for the out-of-scope BR/PID-5 subsetting.
- 33 — subset artifacts keep the full instrument's title/block/form name with no "n of 405" notice.
- 32 — the dual-name no-collision invariant is a comment, not a machine check (verified to hold: 0 collisions across 152 keys).
- 32 — the `subset = NULL` leg of the equivalence test is a tautology.
- 28 — no cross-check that a subset's `instrument` matches the generator's; unreachable while only `"hitopsr"` is constructible.
- 28 — a stale comment and a stale `file:line` cross-reference in two test files.
- 25 — HSUM absent from the `planned` instrument list, so it gets the unknown-value error rather than "not yet supported".
- 22 — the new width expression is less NA/zero-row robust than the old one; unreachable today.
- 20 — factor `scales` rejected rather than coerced.
- 20 — `print.hitop_subset()` does not truncate (77 lines for the all-scales subset).
- 18 — one new test file fails `air format --check` (formatter-caught, excluded by rubric).
- 15 — the CRLF divergence in two R files is perpetuated rather than normalized; the work log records the choice.

### CI and post-fix re-verification

- PR #27 CI green on all 7 jobs before the fix (macOS release, Ubuntu devel/release/oldrel-1, Windows release, pkgdown, test-coverage); re-run after the fix at merge time.
- After fix: `devtools::test()` FAIL 0 | WARN 0 | SKIP 1 | PASS 9779 · `devtools::check()` 0/0/0 · `document()` no diff.
