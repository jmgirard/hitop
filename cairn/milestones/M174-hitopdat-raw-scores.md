# M174: HiTOP-DAT raw scale scores

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, GP1, GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — a new exported scoring function
- **Branch/PR:** —

## Goal

`score_hitopdat()` returns a raw score on each HiTOP-DAT scale other than General Depression, by the rule its measure's source states.

## Scope

**In:**
- `score_hitopdat()` in a new `R/score_hitopdat.R`, reusing the `R/util.R` validators and `round_half_up()`'s file for any helper it needs.
- Sums for the IDAS-II, WHODAS, AUDIT, DUDIT and PHQ-15 scales. Item means for the CAT-PD facets and CAPE positive symptoms. Reverse keys from `hitopdat_scales$reverseNumbers`.
- Proration of every scale with at least `min_complete` of its items answered (default 0.8), and a `_complete` column per scale (the authors' answers, SOURCES.md "The authors' joint answers").
- A refusal of answer values outside an item's answer set, under a new public class.
- Source notes for Andrews et al. (2009), Bergman & Källmén (2002), Berman et al. (2005) and Hinz et al. (2017), the sources of the scoring rules.
- SOURCES.md: DAT-5 and DAT-17 (c), (e) and (h) resolved by the authors' answers.

**Out:**
- T scores and the alert: M175.
- AUDIT and DUDIT T scores and the cutoff flags, CAT-PD domain scores, and scored REDCap and Qualtrics files: the candidate row "HiTOP-DAT after M143 to M145", waiting on the authors or on M175.
- General Depression: the authors dropped it (2026-11-18). Its `hitopdat_scales` row stays, as the IDAS-II key's record.
- A reliability function for the battery. Nobody asked for one.

## Acceptance criteria

- [ ] AC1: `score_hitopdat(data, items, prefix = "dat_", min_complete = 0.8, append = TRUE)` returns a tibble. With `append = FALSE` its names equal, for the 56 rows of `hitopdat_scales` other than General Depression in table order, `prefix` + `camelCase`, followed by `prefix` + `camelCase` + `"_complete"` in the same order. A test compares the names with a list of the 56 stems typed in the test.
- [ ] AC2: On fully answered responses drawn with a fixed seed from each item's `hitopdat_choices` set, plus one row at every set's minimum and one at its maximum, every score equals a recomputation in a test helper. The helper's item lists and reverse keys are typed from the shelved keys: Watson (2011) for the 18 IDAS-II scales, the IPIP CAT-PD-SF v1.1 key for the 33 CAT-PD facets, and the battery's item lists for the WHODAS, AUDIT, DUDIT, CAPE and PHQ-15. The rule is a sum for the IDAS-II scales (the metric of Watson et al., 2012, Table 2), the WHODAS (Andrews et al., 2009, p. 2), the AUDIT (Bergman & Källmén, 2002), the DUDIT (Berman et al., 2005) and the PHQ-15 (Hinz et al., 2017, 0 to 28). It is an item mean for the CAT-PD facets (the metric of the authors' workbook) and CAPE positive symptoms (the metric of the authors' REDCap version). A reversed item scores (min + max) − answer on its answer set.
- [ ] AC3: With items missing, a scale whose answered share is at least `min_complete` scores its answered sum times its item count divided by its answered count (sum scales), or its answered mean (mean scales). A scale below that share scores `NA`. A test checks every scale at the smallest answered count k with k / n ≥ `min_complete` and at k − 1, with a reversed item among the missing where the scale has one. It runs at the default, at `min_complete = 1`, and at `min_complete = 0.7` on Rigidity (10 items), where 0.7 × 10 is not exact in floating point.
- [ ] AC4: Each `_complete` column equals the scale's answered item count divided by its item count, tested on the AC3 fixtures.
- [ ] AC5: An `NA` answer counts as missing. A zero-row `data` returns a zero-row tibble with the AC1 names. `score_hitopdat()` refuses each of these, and a test fires each one and asserts its condition class (or its message, for the unclassed validators in `R/util.R`): `data` not a data frame; `items` not of length 382, holding a duplicate, or naming a column absent from `data`; a non-numeric item column, a factor included (`hitop_nonnumeric_items`); an answer that is not a value of its item's answer set in `hitopdat_choices` (the new public class `hitop_invalid_answer`); `min_complete` not one number in (0, 1]; `prefix` not one string; `append` not `TRUE` or `FALSE`; an output name already in `data` (`hitop_append_collision`).
- [ ] AC6: `?score_hitopdat` states each measure's rule and source, the 80% default as the authors' choice, that the PHQ-15 prorates where Hinz et al. (2017, p. 28) filled one missing item with the other items' rounded mean, and that General Depression is not scored at the authors' request. NEWS.md has an entry, `_pkgdown.yml` lists the function, and the README capability table marks HiTOP-DAT scoring. `devtools::test()` reports 0 failures, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T2, T3, T4
- AC6 → T5

## Tasks

- [ ] T1: Write the source notes `andrews2009.md`, `bergman2002.md`, `berman2005.md` and `hinz2017.md` from the shelved PDFs, each with its sha256 and INDEX line, holding the scoring rule the milestone uses. Resolve DAT-5 and DAT-17 (c), (e) and (h) in SOURCES.md, citing `jonas_ringwald2026email3.txt`.
- [ ] T2: Write `tests/testthat/test-score_hitopdat.R` first, with a helper that types the item lists and reverse keys from the keys (LESSONS 2026-10-04 (M159): random answers with `NA`, never a periodic pattern). Run it on the base and log that it fails for the missing function.
- [ ] T3: Write `score_hitopdat()` in `R/score_hitopdat.R`. Read memberships and reverse keys from `hitopdat_scales` and answer sets from `hitopdat_items` and `hitopdat_choices`; hardcode no item number (CLAUDE.md).
- [ ] T4: Append two D-entries: the signature departs from the family's (`min_complete` in place of `missing`, no `srange`, no `calc_se`), because the battery's answer sets differ by measure and the authors set the proration rule; and the new public class `hitop_invalid_answer` (D-034(c)).
- [ ] T5: Write the roxygen, NEWS entry, `_pkgdown.yml` entry and README row (`devtools::build_readme()`). Run `devtools::document()`, `devtools::test()` and `devtools::check()`.

## Work log

- 2026-11-25: created by /milestone-plan, together with M175.
- 2026-11-25: tier check: milestone — new exported scoring function with design decisions.
- 2026-11-25: idea vet: gap — IDAS-II metric: the REDCap version scores item means and the Qualtrics file and Table 2 sums; reading taken: sums, the metric Table 2 prints, which gives the same T. Gap — CAPE metric: item mean, as both of the authors' versions use. Gap — "report both thresholds" was read as covering cutoffs only; the reading for T scores goes to the authors (candidate row). No flaw: D-098 settles the unpublished-norms concern.
- 2026-11-25: lessons used: 2026-10-04 (M159), 2026-07-31 (M032), 2026-08-21 (M043).
- 2026-11-25: collision check: promotes the parts "scale scores by each measure's rule" and the M143 review's reverse keys and stems of the candidate row "HiTOP-DAT after M143 to M145"; D-098 governs; no open issue overlaps (one open, #87).
- 2026-11-25: question set: how should the parts waiting on the authors wait? Answer: ask them, plan later. Jeff sends the follow-up drafted in this session.
- 2026-11-25: question set: when to plan the scored REDCap and Qualtrics files? Answer: after the R scoring ships (candidate row).
- 2026-11-25: plan chose proration with a minimum share (Jonas 2026-11-18) over the REDCap version's unprorated sums and means, because the latest answer overrides the file. Falsified by a later author answer.
- 2026-11-25: criteria audit (fresh Opus reader, full mode, with M175 and a dropped M176 draft) returned 21 findings. Applied to M174: draws from each answer set with min and max rows (AC2), the CAPE metric cited to the REDCap version (AC2), a 0.7 probe on Rigidity (AC3), the PHQ-15 proration departure stated (AC6), `NA` as missing and zero rows (AC5), the signature departure as a D-entry (T4).
