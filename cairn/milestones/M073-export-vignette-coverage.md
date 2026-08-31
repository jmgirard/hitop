# M073: Every exported function the package still recommends is demonstrated or linked in the vignettes

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** —

## Goal

Close the six gaps between what `NAMESPACE` exports and what the shipped
vignettes and articles demonstrate, and ship the sweep that found them as a
test so the gap does not reopen on the next export.

## Scope

Surface tier: **user-facing** — the deliverable is vignette and article content
that package users read; the sweep that verifies it is internal, the thing it
verifies is not.

**In:** A test-suite sweep classifying every `NAMESPACE` export against every
`.Rmd` under `vignettes/` and `vignettes/articles/`; a `rank_scales()`
demonstration in the full-form PID-5 scoring vignette; `label_hitopsr()` and
`label_hitopbr()` demonstrations in their instruments' scoring vignettes; a
generator card on the HiTOP-HSUM download article, which is the only one of six
carrying none.

**Out:** Worked examples of `calc_se` in the full-form and brief-form PID-5
vignettes — the roadmap row asking for them predates the deprecation, and
demonstrating an argument that warns and is scheduled for removal would teach
readers to call it; dropped at the plan gate, the roadmap row annotated with the
reason rather than promoted. Documenting `hitop_subset()`, deprecated at
`R/deprecated.R:31-44`. Any coverage promise over exported *datasets*, which
`NAMESPACE` does not list → candidate row if a reader reports a gap. Broadening
the HiTOP-HSUM article beyond the generator card while its scoring is pending →
stays with the HiTOP-HSUM scoring candidate row.

## Acceptance criteria

- [ ] AC1: A sweep over every `export()` entry in `NAMESPACE` against every
      `.Rmd` file under `vignettes/` and `vignettes/articles/` reports no export
      that is neither called in an evaluated `{r}` chunk of one of those files,
      nor linked there by an `href` to its own `reference/<name>.html` page, nor
      a function whose own body calls one of `R/util.R`'s `deprecate_*()`
      helpers.
- [ ] AC2: `vignettes/pid5_scoring.Rmd` demonstrates `rank_scales()` in an
      evaluated chunk over scale columns scored earlier in that same vignette,
      with prose naming what the output column holds.
- [ ] AC3: `vignettes/hitopsr_scoring.Rmd` demonstrates `label_hitopsr()` and
      `vignettes/hitopbr_scoring.Rmd` demonstrates `label_hitopbr()`, each in an
      evaluated chunk showing the label attached to a named column.
- [ ] AC4: `vignettes/articles/download-hitophsum.Rmd` carries a card linking
      the reference page of each HiTOP-HSUM generator the package exports.
- [ ] AC5: `devtools::test()` clean, `devtools::document()` no diff,
      `devtools::check()` 0 errors / 0 warnings, `pkgdown::check_pkgdown()`
      passes.

## Coverage

- AC1 → T1, T2, T3, T4, T5
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6

## Tasks

- [ ] T1: Write `tests/testthat/test-vignette-export-coverage.R`. Enumerate the
      `export()` lines of `NAMESPACE`; enumerate `.Rmd` under `vignettes/` and
      `vignettes/articles/`; split each file into evaluated `{r}` chunk bodies
      (skipping `eval = FALSE`) and non-chunk text; classify each export as
      called-in-chunk, linked as `reference/<name>.html`, or exempt because its
      body calls a `deprecate_*()` helper. Skip when `vignettes/` is absent, the
      source-checkout guard at `tests/testthat/test-vignette-se-prose.R:13-15`.
- [ ] T2: Prove the sweep can fail. Plant one defect per arm — an export named
      only in prose, an export called only inside an `eval = FALSE` chunk, and a
      non-deprecated export mentioned nowhere — and record each red. Confirm both
      enumerations are non-empty in the passing run, and include one export the
      change leaves untouched.
- [ ] T3: Add a `rank_scales()` section to `vignettes/pid5_scoring.Rmd` after
      `## Score simulated PID-5 data` (line 30), ranking the scored columns with
      `prefix = "pid_"`, plus prose naming what the output column holds.
- [ ] T4: Add a `label_hitopsr()` demonstration to `vignettes/hitopsr_scoring.Rmd`
      and a `label_hitopbr()` one to `vignettes/hitopbr_scoring.Rmd`, each showing
      the attached label of a named column, modelled on the help-page examples at
      `R/label_hitopsr.R:14-18` and `R/label_hitopbr.R:15-19`.
- [ ] T5: Add a Custom File Generation card to
      `vignettes/articles/download-hitophsum.Rmd`, modelled on
      `vignettes/articles/download-hitopsr.Rmd:67-79` but carrying only the two
      generators HiTOP-HSUM exports.
- [ ] T6: NEWS entry for the added vignette sections and the HiTOP-HSUM page's
      generator links; run `document()`, `test()`, `check()`, `check_pkgdown()`.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in full mode (user-facing tier) in a fresh-context [O] reader that authored none of the criteria; it returned five findings — AC1 satisfiable with the goal unmet ("mentioned by name" admits a comment or a substring), AC4 self-contradictory (a card "matching" five three-button cards for an instrument exporting two generators), AC5 binding a property of the test harness rather than of the vignettes, no criterion citing a probe, and two wrong facts in the draft (10 article `.Rmd` files not 11; `hitop_subset()` at `R/deprecated.R:31-44` not 30-33). All five had one clear right answer and were fixed before these criteria were written.
- 2026-08-30: plan gate chose dropping the `calc_se` half of the roadmap row over adding its worked examples, because the argument warns and is scheduled for removal; falsified by a reader reporting that the full-form or brief-form PID-5 vignette leaves the deprecation unexplained.
- 2026-08-30: plan gate chose the full-form PID-5 vignette for the ranking demonstration over the HiTOP-BR vignette, because the full form's 25 facets make a top-5 ranking illustrative where 8 HiTOP-BR scales do not; falsified by the demonstration needing an argument the full form cannot show.
- 2026-08-30: plan gate chose shipping the sweep as a permanent test over a one-time check, because a one-time check reopens the gap on the next export; falsified by the test failing on an export deliberately left undocumented for a reason the deprecation exemption does not cover.
- 2026-08-30: plan gate chose adding the HiTOP-HSUM generator card over exempting that article, because the page already offers the prebuilt files those generators produce; falsified by a reader taking the card as a claim that HiTOP-HSUM scoring is supported.

## Decisions

## Review
