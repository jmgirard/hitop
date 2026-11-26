# M175: HiTOP-DAT T scores and the suicide-risk alert

- **Status:** review
- **Priority:** normal
- **Depends on:** M174
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, IP4, GP1, GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — a new exported function and dataset
- **Branch/PR:** m175-hitopdat-t-scores

## Goal

`norm_hitopdat()` turns `score_hitopdat()` scores into T scores against the norms the authors name, and marks Suicidality and Self-Harm T scores at or above an alert point.

## Scope

**In:**
- An exported dataset `hitopdat_norms`, built by a `data-raw/` script, for 53 scales: the 18 IDAS-II scales other than General Depression, the WHODAS, CAPE positive symptoms and the 33 CAT-PD facets.
- `norm_hitopdat()` in a new `R/norm_hitopdat.R`, returning whole-number T scores, half away from zero (`round_half_up()`, R/util.R:768), as both of the authors' versions round.
- Alert columns for IDAS-II Suicidality and CAT-PD Self-Harm, at T 65 by default (Jonas, 2026-10-15).
- Source notes for Galletly et al. (2016) and the authors' workbook.

**Out:**
- AUDIT, DUDIT and PHQ-15 results under men's and women's values, CAT-PD domain scores, and scored REDCap and Qualtrics files: the candidate row "HiTOP-DAT after M143 to M145".
- Alerts on other scales. The authors alert on these two only.

## Acceptance criteria

- [ ] AC1: `hitopdat_norms` holds one row for each of the 53 scales, giving the scale, its `camelCase`, M, SD, the reference sample, its N where the source prints one, and the source. A test checks every row against values typed in the test from the committed source notes: Watson et al. (2012) Table 2, "Adults" column (p. 407); Andrews et al. (2009) Table 1, the Total-row, Total-column cell (N 8,824, M 3.1, SD 5.3); Galletly et al. (2016) Table 1, the n-weighted mean of the male (26.26, n 125) and female (26.58, n 180) CAPE positive means and their common printed SD (3.52), each divided by 20; and the authors' workbook, sheet "CAT-PD Scoring", cells E3:F35 ("Community"), at their stored precision.
- [ ] AC2: `norm_hitopdat(data, prefix = "dat_", alert_t = 65, append = TRUE)` reads the score columns `prefix` + `camelCase` for the 53 scales. With `append = FALSE` its names equal a list typed in the test: the 53 `prefix` + `camelCase` + `"_t"` columns in `hitopdat_norms` order, then `prefix` + `"suicidality_alert"` and `prefix` + `"selfHarm_alert"`. Each `_t` equals `round_half_up(50 + 10 * (score - M) / SD)`. A test checks every `_t` column against M and SD typed in the test, at the mean (T 50), at a score whose unrounded T is 0.4 above a whole number, at one 0.6 above, and at `NA`.
- [ ] AC3: Each alert column is `TRUE` when its scale's rounded T is at least `alert_t`, `FALSE` when below, and `NA` when the T is `NA`. A test checks each at unrounded T values of `alert_t` − 0.4 (`TRUE`) and `alert_t` − 0.6 (`FALSE`), at the default and at `alert_t = 70`.
- [ ] AC4: A zero-row `data` returns a zero-row tibble with the AC2 names. `norm_hitopdat()` refuses each of these, and a test fires each one and asserts its condition class or message: `data` not a data frame; a score column absent from `data`; a non-numeric score column (`hitop_nonnumeric_items`); `alert_t` not one finite number; `prefix` not one string; `append` not `TRUE` or `FALSE`; an output name already in `data` (`hitop_append_collision`).
- [ ] AC5: `?norm_hitopdat` and `?hitopdat_norms` name each norm source, say that the CAT-PD norms are the authors' unpublished workbook (D-098), and cite Katherine Jonas's email of 2026-10-15 for T 65. They say that the AUDIT, DUDIT and PHQ-15 get no T score, that the AUDIT and DUDIT wait on the authors' answer about their sex-split norms, and that the authors keep the PHQ-15 on its cutoffs. The source notes for Galletly et al. (2016) and the workbook are committed with their INDEX lines. NEWS.md and `_pkgdown.yml` list the function and the dataset. `devtools::test()` reports 0 failures, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T3, T4
- AC4 → T3, T4
- AC5 → T1, T5

## Tasks

- [x] T1: Write the source notes `galletly2016.md` and `simms2026catpdworkbook.md` (the workbook, sha256 `8643147c…`, the cells AC1 names) with INDEX lines, and add the 18 IDAS-II rows already in `watson2012.md` to its "Traces to".
- [x] T2: Transcribe the workbook's 33 cells into a committed `data-raw/hitopdat_norms.csv` with the other 20 rows, and write `data-raw/hitopdat_norms.R` to build the dataset from it, adding no dependency. Write `?hitopdat_norms` in R/data.R and AC1's test.
- [x] T3: Write `tests/testthat/test-norm_hitopdat.R` first: AC2 to AC4, with M and SD typed in the test, never read from `hitopdat_norms`.
- [x] T4: Write `norm_hitopdat()` in `R/norm_hitopdat.R`, reusing the `R/util.R` validators.
- [x] T5: Write the roxygen, NEWS entry and `_pkgdown.yml` entries. Run `devtools::document()`, `devtools::test()` and `devtools::check()`.

## Work log

- 2026-11-25: created by /milestone-plan, together with M174.
- 2026-11-25: tier check: milestone — new exported function and dataset.
- 2026-11-25: idea vet: gap — T rounding: whole numbers, half away from zero, as both of the authors' versions round (D-098). Gap — alert on the rounded T, as both versions compare. Gap — CAPE SD: the common printed SD, from Jonas's "weighted mean of the male and female values", over a pooled SD the package would choose. No flaw.
- 2026-11-25: lessons used: 2026-08-22 (M046), 2026-07-31 (M035).
- 2026-11-25: collision check: promotes the parts "T scores against the norms the authors name" and the alert of the candidate row "HiTOP-DAT after M143 to M145"; D-022 holds, since every M and SD is an author-named printed or workbook value, not fitted here.
- 2026-11-25: plan chose whole-number T scores (both of the authors' versions) over unrounded T, because D-098 follows the authors. Falsified by an author answer asking for decimals.
- 2026-11-25: plan chose a committed CSV transcribed from the workbook over reading the workbook with {readxl} in `data-raw/`, because {readxl} is no dependency and the shelf copy is gitignored. Falsified by a workbook revision with many changed cells.
- 2026-11-25: plan chose the workbook's stored precision for the CAT-PD norms over the two decimals the REDCap version types, because the workbook is the source the authors name. Falsified by an author answer naming the two-decimal values.
- 2026-11-25: criteria audit (fresh Opus reader, full mode, with M174) returned 21 findings. Applied to M175: the CAPE SD as the common printed SD (AC1), the workbook sheet and cells and the Andrews cell (AC1), no `scores` argument and an exact name list (AC2), no exact .5 fixture (AC2), the rounded T for alerts with ±0.4/0.6 probes (AC3), the non-numeric class and zero rows (AC4), the email in place of SOURCES.md in the help (AC5). The M176 draft's findings (Berman's women's column ends at raw 3, REDCap's "70+" cap, both-sexes T as a reading, naming, rounding before lookup) went to the authors' follow-up.
- 2026-11-25: implement started on branch m175-hitopdat-t-scores, cut from origin/main at 0e14453b; the six untracked devel/hitopdat_* files are unrelated and stay unstaged.
- 2026-11-25: T1 done: galletly2016.md and simms2026catpdworkbook.md written with INDEX lines; watson2012.md, andrews2009.md and SOURCES.md "The authors' email" updated to name hitopdat_norms as the reader.
- 2026-11-25: T2 done: data-raw/hitopdat_norms.csv + .R build `hitopdat_norms` (53 rows; columns Measure, Scale, camelCase, M, SD, Sample, N, Source); the CAPE row is computed in the script from Galletly's four cells, since the weighted mean repeats. Choices: N 305 for the CAPE (the sum of the printed n), NA for the CAT-PD (workbook gives none); a Measure column added for filtering; the typed norms live in tests/testthat/helper-hitopdat-norms.R so T3's test shares them. A 1e-6 planted change to one M fails the AC1 check.
- 2026-11-25: T3 done: test-norm_hitopdat.R written first and seen red (function absent), then green after T4. Scores are built from the typed M and SD at unrounded T 50, 60.4, 60.6 and NA (expected 50, 60, 61, NA); alerts at alert_t − 0.4 and − 0.6 at 65 and 70, plus a row showing each alert reads only its own scale.
- 2026-11-25: T4 done: `norm_hitopdat()` in R/norm_hitopdat.R. Choices: T columns stay double (AC2's `round_half_up()` result), not integer as in `norm_pid5()`; a missing score column aborts with a message naming the columns, unclassed; `validate_numeric_columns()` gained an optional `class` so the non-numeric refusal carries `hitop_nonnumeric_items`; `alert_t` is checked inline (one finite number, any sign). The append-collision and error-prose sweeps gained a `norm_hitopdat` probe and terminator, and the help page's Errors passage uses the sweep's collision wording.
- 2026-11-25: claim audit: 59 claims read, 5 corrected — R/norm_hitopdat.R, R/data.R, data-raw/hitopdat_norms.R, NEWS.md. Four sites said the PHQ-15 gets no T score "yet"; Jonas's 2026-11-18 answer keeps it on its cutoffs, so they now say no T score, with the AUDIT and DUDIT waiting on the sex-split answer (AC5's "yet" holds for those two). One code comment credited the half-away-from-zero rule to the authors' files, which only call round(x, 0); the Scope line's "as both of the authors' versions round" is right for whole numbers and left as planned.
- 2026-11-25: full `devtools::test()` (run after the sweep fixes): 1 failure, test-vignette-export-coverage — `norm_hitopdat` had no vignette call or reference link. Added a "T scores" reference button to the scoring card of vignettes/articles/download-hitopdat.Rmd (minor sub-task under T5); that test and test-download-pages now pass.
- 2026-11-25: T5 done: roxygen, NEWS entry, `_pkgdown.yml` (norm_hitopdat under Norms, hitopdat_norms with the HiTOP-DAT data), README line pointing to `?norm_hitopdat`. Full `devtools::test()` 0 failures after the vignette link (the one failing file re-run green); `devtools::check()` 0 errors, 0 warnings, 1 note (a gitignored top-level Rplots.pdf left by this session's `run_examples()`, since removed). Status → review.
- 2026-11-25: review pass 1: four lenses spawned; fix-now items committed on the branch (Review section). Full test and check clean at 99acb92d, before those fixes.
- 2026-11-25: amendment routed: AC5 — "They say that the AUDIT, DUDIT and PHQ-15 get no T score yet" is false for the PHQ-15: Jonas's 2026-11-18 answer keeps it on its cutoffs, and the help pages now say so. Status → in-progress for the AC5 wording amendment alone.
- 2026-11-25: re-audit: AC5 (full) — no blocking finding; the amended text holds on the branch at fd6a9a2c; it widens AC5 slightly (adds the AUDIT/DUDIT reason and the PHQ-15 cutoffs), which the reader judged proportionate and recommended keeping; "the authors keep the PHQ-15 on its cutoffs" rests on Jonas's 2026-11-18 line, Ringwald not objecting in that thread; the INDEX line's "sent" became "received". Kept as audited; no defect return is on record, so D-118 does not bind.
- 2026-11-25: amendment return: AC5 — "They say that the AUDIT, DUDIT and PHQ-15 get no T score, that the AUDIT and DUDIT wait on the authors' answer about their sex-split norms, and that the authors keep the PHQ-15 on its cutoffs."

## Review

Pass 1 (2026-11-25). Default branch unmoved since the cut (origin/main 0e14453b); no PR; Copilot round off.

- gate: `cairn_validate.py` exit 0 (34 advisory warnings, all from before this branch). Full `devtools::test()` at 99acb92d: 0 failures. `devtools::check()` at 99acb92d: 0 errors, 0 warnings, 0 notes.
- spawned: diff-bug, blame-history, prior-review, ripple
- diff-bug #1: two Sample strings (WHODAS, IDAS-II) appear in no committed source note, though AC1's test types them — fix now (added to andrews2009.md p. 2 and p. 4, watson2012.md pp. 404-405), fixed 99acb92d
- diff-bug #2: the WHODAS and CAPE Sample strings were checked only as non-NA — fix now (whole Sample column now typed in test-hitopdat_norms.R)
- diff-bug #3: CAPE N 305 is a sum Galletly does not print, against AC1's "its N where the source prints one" — fix now (CAPE N is NA; the n's stay in `Sample`)
- diff-bug #4: AC5's "get no T score yet" is false for the PHQ-15, which the authors keep on its cutoffs (Jonas, 2026-11-18) — amendment return (AC5 wording; see work log)
- diff-bug #5: the missing-column abort truncates its list at 20 names — fix now (listed in full, as validate_no_output_collision() does; test with a wrong prefix)
- diff-bug #6: `_t` columns are double where norm_pid5() returns integer — reject, planned change (AC2 fixes each `_t` as `round_half_up()`'s result)
- diff-bug #7: `alert_t` checked inline rather than by an R/util.R validator — reject, style
- diff-bug #8: an `Inf` score gives T `Inf` and alert TRUE — reject, false (that is the formula's value; score_hitopdat() cannot produce Inf)
- diff-bug #9: the authors' REDCap version scores IDAS-II item means against Table 2 / item count at two decimals, so 53 of 414 raw sums differ by one T point — reject, planned change (D-098: the named source, Table 2, on the sum metric score_hitopdat() uses)
- diff-bug #10: the signature drops norm_pid5()'s `scores` argument with no D-entry — reject, planned change (AC2 fixes the signature)
- diff-bug #11: no breach of D-022/D-024/D-025 — noted
- diff-bug (c): duplicate or derived test checks (two expects on one missing-column path, a five-value `alert_t` loop over one check, the repeated default-alert block, `whodas_t` in the prefix test, a comment restating `scores_at()`) — fix now (trimmed); the explicit collision and zero-row class checks and the 53-row and Measure/Scale checks stay — reject, planned change (AC1 and AC4 name them)
- blame-history #1: IDAS-II norms differ from the authors' file (DAT-15 (b)) with no answer — reject, false (Jonas's 2026-11-07 answer names Watson Table 2; SOURCES.md "The authors' second answers")
- blame-history #2: the CAPE mean is computed here, against D-022 — reject, planned change (AC1 names the weighted mean; D-098 admits author-named arithmetic on printed cells)
- blame-history #3: the references say the workbook was "Sent" 2026-10-15, but that date is the download time — fix now ("Received by the package maintainer … 2026-10-15" in R/data.R and R/norm_hitopdat.R)
- blame-history #4: `hitop_nonnumeric_items` extended past D-068's item-scoring functions — reject, planned change (AC4 names the class)
- blame-history #5: README capability table not checked — reject, false (test-readme-capabilities.R passes; Scoring was already "Yes")
- prior-review #1: a NaN score gives T NaN, not the documented NA — fix now (NaN set to NA before rounding; test added)
- prior-review #2: duplicated score-column names read the first match, as in norm_pid5() and rank_scales() — known issue: **Norming and ranking functions read the first of duplicated score column names** — `norm_hitopdat()`, `norm_pid5()` and `rank_scales()` select score columns by name, so a data frame holding two columns of one name (possible with `check.names = FALSE`, refused by tibble) reads the first with no message. Accepted at the M175 gate
- prior-review #3: duplicate names already in `data` reach tibble's error under append — reject, edge (a data frame with duplicated names, outside documented and common use; the effect is an error)
- prior-review #4: `round_half_up()` may miss a half in floating point — reject, false (diff-bug lens checked every score score_hitopdat() can give at ≥ 80% answered: the three exact halves are exact doubles)
- ripple #1, #5: DESIGN.md said HiTOP-DAT T scores "are not built yet" and omitted `norm_hitopdat()` — fix now, fixed e31cf051
- ripple #2: NEWS "Three datasets hold the HiTOP-DAT" — fix now (reworded to the items, answer sets and scales), fixed e31cf051
- ripple #3: SOURCES.md said no table reads the second answers — fix now, fixed e31cf051
- ripple #4: `hitopdat_norms` not in `utils::globalVariables()` — reject, false (`check()` reports no undefined global)
- ripple #6: ROADMAP's PHQ-15 wording against NEWS — reject, false (the row concerns cutoffs under men's and women's values, which the authors keep)
- ripple #7: CLAUDE.md's opening line omits the HiTOP-DAT — follow-up, absorbed: candidate row "HiTOP-DAT after M143 to M145" item (e) already holds it
- ripple #8: test-error-prose.R says "three" selection exports where four are listed — fix now (a count in a file this diff edits, beside counts the diff corrected)
- ripple #9: jonas2021dat.md pointed T-score material at the candidate row — fix now, fixed e31cf051
