# M082: `rename_pid5_items()`

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Branch/PR:** `m082-rename-pid5-items` / https://github.com/jmgirard/hitop/pull/89

## Goal

Ship `rename_pid5_items()`, so PID-5 data named elsewhere — or named by this
package before D-055 — can be renamed into the pattern D-055 fixed.

## Scope

Surface tier: **user-facing** — a new exported function with documented
arguments, defaults and reports.

**In:**

- `rename_pid5_items(data, version, method, item_cols, item_text, from_prefix,
  prefix)`. `method = "text"` matches item prompts against `pid_items$Text`
  restricted to the named form, as `rename_hitopsr_items()` does.
  `method = "number"` renames columns spelled `<from_prefix><digits>` to the
  canonical padded name for the same item number, `from_prefix` defaulting to
  `"pid_"` — the spelling the package itself shipped until D-055.
- One public condition class for the unmatched-item report of both methods,
  with the `cairn/DECISIONS.md` entry D-034(c) requires.
- Roxygen docs with a runnable example, tests, a NEWS bullet, a `_pkgdown.yml`
  reference entry, and a one-line mention in each PID-5 scoring vignette.

**Out:**

- `label_pid5()` → M083, planned in the same run.
- A legacy-name (`method = "original"`) route → not plannable: `pid_items` has
  no `Original` column and no PID-5 legacy item pool is on the `references/`
  shelf.
- Inferring `version` from the data rather than taking it as an argument →
  candidate row.
- Retrofitting `rename_hitopsr_items()`'s classless unmatched-text warning to
  the new class → candidate row; declined at this plan gate as a second
  breaking change.

## Acceptance criteria

- [x] AC1: `rename_pid5_items()` is exported and, for each of
      `version = "FULL"`, `"SF"` and `"BF"`, `method = "text"` renames every one
      of that form's items to its canonical padded name. Evidence: a test that,
      per version, selects the rows of `pid_items` whose column for that form is
      non-`NA`, builds a data frame with one arbitrarily-named column per
      selected row, passes those column names as `item_cols` and those rows'
      `Text` as `item_text`, and asserts the returned `colnames()` are
      `identical()` to `item_names(prefix, <those rows' form numbers>,
      max_n = <the form's largest number>)` — 220, 100 and 25 columns.
- [x] AC2: With `method = "number"`, a column spelled `<from_prefix><digits>`
      whose number names an item of the form is renamed to that item's canonical
      padded name; a column spelled `<from_prefix><digits>` whose number names no
      item of the form keeps its name and is named in the report; a column not
      spelled `<from_prefix><digits>` keeps its name and is named in no report.
      Evidence: a test that, per version, feeds a frame holding one column per
      item of the form named `<from_prefix><unpadded number>`; one column per
      member of an enumerated matching-but-unnamed family — a number above the
      form's largest, the number `0`, and (for SF and BF) a number that names a
      FULL item but no item of this form; and one column per member of an
      enumerated non-matching family — the canonical padded name itself,
      `<from_prefix>` with a non-digit suffix, the stem as a substring of a
      longer name, and a differing-case spelling of `<from_prefix>` — then
      asserts each item column's returned name is `identical()` to its canonical
      name, every other name is unchanged, and the report names exactly the
      matching-but-unnamed columns.
- [x] AC3: Under `method = "text"`, an `item_text` entry matching no item of the
      named form is skipped, the column it referred to keeps its name, and a
      condition of this milestone's new class is signalled naming it; the
      `method = "number"` report is AC2's. Evidence: a test, per version,
      planting among matching entries each member of an enumerated unmatchable
      family — a wholly foreign string, and the `Text` of a `pid_items` row whose
      column for this form is `NA` — and asserting the condition's class, that
      the report names each planted entry, and that the corresponding columns'
      names are unchanged; and a positive control in the same test that an
      entry differing from a real item's `Text` only in surrounding whitespace is
      matched and renamed, `rename_hitopsr_items()` comparing under `trimws()`
      (`R/rename_hitopsr_items.R:81`), so that the family above is not vacuous.
- [x] AC4: `prefix = NULL` resolves to `"pid5_"`, `"pid5sf_"` and `"pid5bf_"`
      under `version = "FULL"`, `"SF"` and `"BF"`, and `from_prefix` defaults to
      `"pid_"`. Evidence: a test asserting, for all three versions, that a
      `method = "text"` call with neither argument given returns `colnames()`
      `identical()` to a literal expected vector written in the test, and that a
      `method = "number"` call with neither argument given, over columns spelled
      `pid_<number>`, does the same — the second call renaming nothing unless
      `from_prefix` defaults as promised. Neither expectation is re-derived by
      calling the function.
- [x] AC5: The function is documented with a runnable roxygen example, carries
      one `NEWS.md` bullet under the development-version heading stating what it
      does, has a `_pkgdown.yml` reference entry, and is named once in each of
      `vignettes/pid5_scoring.Rmd`, `pid5sf_scoring.Rmd` and
      `pid5bf_scoring.Rmd`.
- [x] AC6: The active profile's verify and review checks are clean:
      `devtools::test()` passes, `devtools::document()` leaves no diff,
      `devtools::check()` reports 0 errors and 0 warnings with any NOTE
      justified, and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T1, T2, T3
- AC4 → T2, T3
- AC5 → T4
- AC6 → T4, T5

## Tasks

- [x] T1: Append the `cairn/DECISIONS.md` entry D-034(c) requires for the new
      unmatched-item condition class, naming the class and both methods that
      raise it; add the internal raiser beside `warn_unpadded_items()`
      (`R/util.R:595`).
- [x] T2: Write `tests/testthat/test-rename_pid5_items.R` red — AC1's per-form
      text sweep, AC2's number method with its enumerated non-matching family,
      AC3's unmatchable family per method, AC4's literal-expectation defaults.
- [x] T3: Implement `R/rename_pid5_items.R`, modelled on
      `R/rename_hitopsr_items.R`, with the `version` resolution `score_pid5()`
      uses (`R/score_pid5.R:142`) and `item_names()` for the padded names; run T2
      green.
- [x] T4: Roxygen docs and example, `document()`, the NEWS bullet, the
      `_pkgdown.yml` entry, and the three vignette mentions.
- [x] T5: Run the profile's verify and review checks; record the output.

## Work log

- 2026-09-02: created by /milestone-plan, alongside M083.
- 2026-09-02: criteria audit ran in FULL mode (user-facing tier); a fresh-context reader returned findings on 7 of the 10 criteria drafted for the unsplit scope. Fixed before the gate: AC1's test omitted `item_cols`, which the sibling aborts without; the scale-label criterion promised a domain including `calc_se` `_se` columns its evidence never enumerated; the default-`prefix` criterion was self-comparing and satisfiable by an implementation ignoring `prefix`; the unpadded criterion left `target` unstated and probed one spelling of two; a drafted NAMESPACE/`_pkgdown.yml` parity criterion bound an instrument act ("run and shown") and duplicated `pkgdown::check_pkgdown()`, so it was dropped into the gate criterion; the drafted NEWS criterion named no checkable state. Posed at the gate: AC2's out-of-form-number rule and AC3's condition class.
- 2026-09-02: re-audited after the gate; four further findings, three clear-cut, all fixed before commit. AC2 put a number above the form's largest into its non-matching family when that column does match the pattern, and asserted no absence of a report — the family was split into matching-but-unnamed and non-matching and the report's contents pinned exactly. AC3 listed a whitespace near-miss as unmatchable where `rename_hitopsr_items()` compares under `trimws()`, so the criterion demanded a state its own task made unreachable — the near-miss became a positive control, and the number method's report moved to AC2, having no text-shaped unmatchable family. AC4 promised a `from_prefix` default that its single text-method call never consults — a `method = "number"` call was added.
- 2026-09-02: sizing tripwire fired on the unsplit scope (9 acceptance criteria, 7 tasks); split into M082 and M083 rather than compressed, the two functions having disjoint implementations, test files and failure modes.
- 2026-09-02: plan gate chose `method = c("text", "number")` over text-only because D-055 renamed `pid_1` to `pid5_001` with no in-package migration path; falsified by users reaching for the text method to migrate package-named data.
- 2026-09-02: plan gate chose a public condition class with a D-entry over the classless `cli_warn()` the HiTOP sibling uses because a test can otherwise only assert on message prose; falsified by the class going unused by any caller while the sibling's classless report causes no trouble.
- 2026-09-02: implementation gate chose `method = "number"` as the default (the migration from the package's own pre-rename spelling), the condition class name `hitop_unmatched_items`, and keeping the sibling's completeness warning on both methods.
- 2026-09-02: T1 — D-057 appended; `warn_unmatched_items()` added beside `warn_unpadded_items()` in `R/util.R`, raising the class for both methods and doubling braces in caller-supplied bullet text so cli does not read one as an expression.
- 2026-09-02: T2/T3 — `tests/testthat/test-rename_pid5_items.R` written red (function absent), then `R/rename_pid5_items.R` implemented green. Four planted defects each went red: a wrong `from_prefix` default, a report naming every pattern-matching column, a pad width fixed at 220, and text matched against all of `pid_items` rather than the named form's rows. The second exposed AC2's report check as too loose — it never asserted the renamed item columns were absent — so the check now compares the report's bullets as a set.
- 2026-09-02: AC3's off-form-text member is empty for `version = "FULL"`, every `pid_items` row carrying a FULL number; the test asserts that emptiness holds for FULL and for no other form rather than skipping it silently.
- 2026-09-02: T4 — roxygen docs and example, `document()`, the NEWS bullet, the `_pkgdown.yml` Utilities entry, and one mention in each of the three PID-5 scoring vignettes.
- 2026-09-02: checkpoint at T1-T4. `test-rename_pid5_items.R` green on its own and four planted defects each red; the full `devtools::test()` run was still in flight at the commit, so T5 remains open.
- 2026-09-02: the full suite's export-coverage guard went red on `rename_pid5_items()` — it counts a call in an evaluated vignette chunk or a reference link, not a prose mention, so AC5's naming alone does not satisfy it. The full-form vignette's mention became a worked chunk renaming `pid_1`/`pid_2`; the short- and brief-form mentions stay prose.
- 2026-09-02: T5 — `devtools::test()` FAIL 0 | WARN 0 | SKIP 9 | PASS 16502; `devtools::check()` 0 errors, 0 warnings, 0 notes (5m 38s); `pkgdown::check_pkgdown()` no problems; `devtools::document()` leaves no diff. Status set to review.
- 2026-09-02: review opened; draft PR #89 created, main unmoved since the branch was cut. `cairn_validate.py` passes (exit 0). Acceptance-criterion evidence pending the in-flight `devtools::test()`/`check()` run and the three review lenses.
- 2026-09-02: review evidence recorded for AC1-AC6, all met; consistency gate clean. Three review lenses: prior-review and blame-history clean, the diff-bug lens returned 16 ranked findings, dispositions proposed in the Review section and pending the gate. No finding meets the return floor.
- 2026-09-02: merge approved at the gate with the seven fix-now findings directed; applied on the branch and the profile's checks re-run clean (PASS 16504, check 0/0/0).
- 2026-09-02: merge approval recorded in `cairn/.merge-approved` for PR #89; PR marked ready. The CI watcher hit the harness ceiling at 540s with seven checks still pending (`line endings` passed); watcher stopped, nothing merged. Resume by re-reading `gh pr checks 89`.

## Decisions

## Review

- AC1 — met. `devtools::test()` on 2026-09-02: FAIL 0 | WARN 0 | SKIP 9 | PASS 16502, `test-rename_pid5_items.R` among them. Its first test sweeps `method = "text"` per form and compares `colnames()` with `item_names()` under `expect_identical()`, plus a guard asserting the three form widths are 220/100/25 so the sweep is not vacuous.

- AC2 — met. The second test in `test-rename_pid5_items.R` passes in the run above. Per form it feeds one `pid_<unpadded number>` column per item, the matching-but-unnamed family (`max_n + 1`, `0`, and for SF/BF the numbers 150 and 100), and the four non-matching spellings; it compares the item block and the remainder with `expect_identical()` and the report bullets as a set with `expect_setequal()` plus `expect_length()`. Checked at review: each form numbers 1..N contiguously (`identical(sort(n), seq_len(max(n)))` TRUE for all three), so SF 150 and BF 100 do name a FULL item and no item of their own form — the third family member is instantiated, though it coincides with the first.

- AC3 — met. The third test passes in the run above: per form it plants a foreign string and, for SF/BF, the `Text` of a row the form does not carry, asserts `expect_s3_class(caught, "hitop_unmatched_items")`, matches each planted entry in the report, and compares all `colnames()` with `expect_identical()`. The whitespace-padded positive control renames, so the unmatchable family is not vacuous; the FULL form has no off-form text and the test asserts that emptiness holds for FULL and no other form.

- AC4 — met. The fourth test passes in the run above, comparing `colnames()` with the literal vectors `pid5_001/002/220`, `pid5sf_001/002/100` and `pid5bf_01/02/25` under `expect_identical()` for a `method = "text"` call and a `method = "number"` call over `pid_<number>` columns, neither giving `prefix` or `from_prefix`. Expectations are literals, not re-derived from the function.

- AC5 — met. `NAMESPACE:39` exports it; `man/rename_pid5_items.Rd` exists with an `@examples` block that `R CMD check`'s "checking examples" step ran OK; one bullet sits under the `# hitop (development version)` heading of `NEWS.md`; `_pkgdown.yml:98` carries the reference entry; and `rename_pid5_items` appears in `vignettes/pid5_scoring.Rmd` (prose plus a worked chunk), `pid5sf_scoring.Rmd:30` and `pid5bf_scoring.Rmd:30`. Judgment recorded: the bullet exists, sits under the right heading and describes the function, so the criterion is met — one clause of it overstates the report, logged below as finding 4 and triaged rather than treated as an AC failure.

- AC6 — met. Fresh run on 2026-09-02 from the branch tip: `devtools::test()` FAIL 0 | WARN 0 | SKIP 9 | PASS 16502; `devtools::document()` left `git status` empty; `pkgdown::check_pkgdown()` "No problems found"; `devtools::check()` Status OK, 0 errors | 0 warnings | 0 notes (7m 8s).

### Consistency gate

- Universal: `cairn_validate.py` exit 0, all checks PASS, 24 advisories (23 dangling pre-migration D-ids, 1 references-staleness) — none a gate failure, none introduced by this branch. `release window` OK. No `DESIGN.md` principle changed, so `cairn_impact.py` was skipped.
- Toolchain (`r-package` consistency-gate slot): `document()` no diff; no hand-edits to `NAMESPACE`/`man/`/`data/*.rda` (the no-diff run covers it); `README.Rmd` untouched; `pkgdown::check_pkgdown()` clean; `NEWS.md` carries the user-visible entry; no new top-level files, so no `.Rbuildignore` entry owed; `devtools::check()` 0/0/0.

### Independent review

Three fresh-context lenses, distinct evidence bases. [S] prior-review: no prior-review evidence on GitHub (`gh api .../pulls/comments?per_page=1` empty), archived `## Review` sections on the touched files checked — no regression of a past lesson, zero findings. [S] blame-history: no blocking or moderate findings; two informational notes (the sibling `rename_hitopsr_items()` passes unmatched text to cli unescaped where the new helper escapes braces — pre-existing and covered by D-057's recorded unevenness; and the AC boxes were unticked at spawn time, which is the expected pre-gate state). [O] diff-bug: 16 ranked findings, below.

Findings and proposed dispositions (verified against the implementation at review, not against the reviewer's account):

1. `method = "number"` reads the digits as the *named form's* item number, so SF/BF data numbered by FULL item numbers is renamed to the wrong columns with no report — every number 1..100 is a valid SF number. Nothing in the roxygen, NEWS or the vignettes says which numbering the digits are read as. Proposed: fix now, one documented sentence.
2. Duplicate column names are produced silently: `data.frame(pid_1 = 1, pid5_001 = 2)` returns two columns named `pid5_001` (verified). Proposed: follow-up.
3. Replacing `numbers %in% form_numbers` with a `1 <= n <= max_n` range check leaves the test file green, and AC2's third family member is said to be unrealizable. Proposed: reject. Verified here that each form numbers 1..N contiguously, so the two predicates are extensionally equivalent — no input distinguishes them, and nothing is left untested; and SF 150 / BF 100 do instantiate the third member, so AC2's evidence is met.
4. The NEWS bullet says item text or columns it cannot match "are left alone and reported under the condition class `hitop_unmatched_items`". Under `method = "number"` an unshaped column such as `age` is left alone and never reported, and the no-column-matches path raises a classless `rlang_warning` (both verified). Proposed: fix now, narrow the wording.
5. `hitop_unmatched_items` is a public contract under D-057 but appears in no `@return`; the package documents `hitop_unpadded_items` that way (`man/label_hitopsr.Rd:21`). Proposed: fix now.
6. `item_cols` and `item_text` are silently ignored under the default `method = "number"` — a caller copying the `rename_hitopsr_items()` idiom gets no error and no rename (verified). Proposed: follow-up.
7. `pid_99999999999` leaks base R's "NAs introduced by coercion to integer range" beside the classed report, against the cli-only messaging convention. Proposed: follow-up.
8. `n_items <- switch(version, "FULL" = 220L, ...)` hardcodes counts already derived two lines earlier as `length(form_numbers)`, against CLAUDE.md's never-hardcode rule. Proposed: fix now.
9. Duplicate entries in `item_cols` drop a mapping unreported; inherited from the sibling. Proposed: follow-up.
10. `prefix` and `from_prefix` are never exercised at a non-default value, so the regex-escaping branch has no test. Proposed: fix now, add the test.
11. The regex-escaping expression is duplicated between `unpadded_item_cols()` in `R/util.R` and `R/rename_pid5_items.R`. Proposed: follow-up.
12. The example comment reads "before 0.3.0"; DESCRIPTION is 0.2.0 and the rename lands in the unreleased development version. Proposed: fix now.
13. AC3's `expect_match(..., fixed = TRUE)` is coupled to cli's 80-column wrapping — today's planted text is 60 characters but `pid_items$Text` runs to 98. Proposed: follow-up.
14. The no-column-matches early return raises a classless warning and skips the completeness check; D-057 describes one class over both methods' unmatched reports. Proposed: follow-up.
15. `NA` in `item_text` is reported as the bare bullet `NA`, indistinguishable from an item whose text is "NA". Proposed: follow-up, with 14.
16. `expect_error(rename_pid5_items(df, version = "XL"), "arg")` matches nearly any error containing "arg" rather than pinning the `match.arg` failure. Proposed: fix now.

Return floor: no finding demonstrates an acceptance criterion failing. Finding 4 was weighed against AC5 and judged not a failure of it — AC5 promises one bullet under the development-version heading stating what the function does, and that bullet exists and describes it; one clause of it overstates the report, which is a defect in the bullet, not an absent artifact. Recorded so the maintainer can overrule.

### Gate triage and fix-now work

Maintainer's disposition at the 2026-09-02 merge gate: fix findings 1, 4, 5, 8, 10, 12 and 16 on the branch, then merge; reject finding 3; defer 2, 6, 7, 9, 11, 13, 14 and 15 to the PID-5 helpers candidate row. Applied:

- 1: `@param method` now states that the digits are read as an item number of the form named by `version`, with the `pid_7` short-form example and the two ways out.
- 4: the NEWS bullet now says item text and columns numbered outside the form are named in the warning, and that a column looking like no item at all is left alone without comment.
- 5: `@return` names `hitop_unmatched_items`, what raises it under each method, and that callers may catch or suppress by class.
- 8: `n_items <- length(form_numbers)` replaces the hardcoded 220/100/25 switch.
- 10: a new test honours `prefix = "x."` on BF and `from_prefix = "pid."` over `pid.1`, `pid.25` and `pidX1`. Discrimination shown: dropping the regex escaping so `.` matches any character turns it red (FAIL 1), restored green.
- 12: the example comment now reads "before the rename" rather than naming 0.3.0.
- 16: the `match.arg` test pins `'should be one of "FULL", "SF", "BF"'` under `fixed = TRUE`.

Re-verified after the fixes: `devtools::document()` wrote `rename_pid5_items.Rd` and left nothing else; `devtools::test()` FAIL 0 | WARN 0 | SKIP 9 | PASS 16504; `pkgdown::check_pkgdown()` no problems; `devtools::check()` Status OK, 0 errors | 0 warnings | 0 notes (6m 23s).
