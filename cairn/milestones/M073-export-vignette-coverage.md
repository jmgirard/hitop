# M073: Every exported function the package still recommends is demonstrated or linked in the vignettes

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m073-export-vignette-coverage` / https://github.com/jmgirard/hitop/pull/79

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

- [x] AC1: A sweep over every `export()` entry in `NAMESPACE` against every
      `.Rmd` file under `vignettes/` and `vignettes/articles/` reports no export
      that is neither called in an evaluated `{r}` chunk of one of those files,
      nor linked there by an `href` to its own `reference/<name>.html` page, nor
      a function whose own body calls one of `R/util.R`'s `deprecate_*()`
      helpers.
- [x] AC2: `vignettes/pid5_scoring.Rmd` demonstrates `rank_scales()` in an
      evaluated chunk over scale columns scored earlier in that same vignette,
      with prose naming what the output column holds.
- [x] AC3: `vignettes/hitopsr_scoring.Rmd` demonstrates `label_hitopsr()` and
      `vignettes/hitopbr_scoring.Rmd` demonstrates `label_hitopbr()`, each in an
      evaluated chunk showing the label attached to a named column.
- [x] AC4: `vignettes/articles/download-hitophsum.Rmd` carries a card linking
      the reference page of each HiTOP-HSUM generator the package exports.
- [x] AC5: `devtools::test()` clean, `devtools::document()` no diff,
      `devtools::check()` 0 errors / 0 warnings, `pkgdown::check_pkgdown()`
      passes.

## Coverage

- AC1 → T1, T2, T3, T4, T5
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6

## Tasks

- [x] T1: Write `tests/testthat/test-vignette-export-coverage.R`. Enumerate the
      `export()` lines of `NAMESPACE`; enumerate `.Rmd` under `vignettes/` and
      `vignettes/articles/`; split each file into evaluated `{r}` chunk bodies
      (skipping `eval = FALSE`) and non-chunk text; classify each export as
      called-in-chunk, linked as `reference/<name>.html`, or exempt because its
      body calls a `deprecate_*()` helper. Skip when `vignettes/` is absent, the
      source-checkout guard at `tests/testthat/test-vignette-se-prose.R:13-15`.
- [x] T2: Prove the sweep can fail. Plant one defect per arm — an export named
      only in prose, an export called only inside an `eval = FALSE` chunk, and a
      non-deprecated export mentioned nowhere — and record each red. Confirm both
      enumerations are non-empty in the passing run, and include one export the
      change leaves untouched.
- [x] T3: Add a `rank_scales()` section to `vignettes/pid5_scoring.Rmd` after
      `## Score simulated PID-5 data` (line 30), ranking the scored columns with
      `prefix = "pid_"`, plus prose naming what the output column holds.
- [x] T4: Add a `label_hitopsr()` demonstration to `vignettes/hitopsr_scoring.Rmd`
      and a `label_hitopbr()` one to `vignettes/hitopbr_scoring.Rmd`, each showing
      the attached label of a named column, modelled on the help-page examples at
      `R/label_hitopsr.R:14-18` and `R/label_hitopbr.R:15-19`.
- [x] T5: Add a Custom File Generation card to
      `vignettes/articles/download-hitophsum.Rmd`, modelled on
      `vignettes/articles/download-hitopsr.Rmd:67-79` but carrying only the two
      generators HiTOP-HSUM exports.
- [x] T6: NEWS entry for the added vignette sections and the HiTOP-HSUM page's
      generator links; run `document()`, `test()`, `check()`, `check_pkgdown()`.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in full mode (user-facing tier) in a fresh-context [O] reader that authored none of the criteria; it returned five findings — AC1 satisfiable with the goal unmet ("mentioned by name" admits a comment or a substring), AC4 self-contradictory (a card "matching" five three-button cards for an instrument exporting two generators), AC5 binding a property of the test harness rather than of the vignettes, no criterion citing a probe, and two wrong facts in the draft (10 article `.Rmd` files not 11; `hitop_subset()` at `R/deprecated.R:31-44` not 30-33). All five had one clear right answer and were fixed before these criteria were written.
- 2026-08-30: plan gate chose dropping the `calc_se` half of the roadmap row over adding its worked examples, because the argument warns and is scheduled for removal; falsified by a reader reporting that the full-form or brief-form PID-5 vignette leaves the deprecation unexplained.
- 2026-08-30: plan gate chose the full-form PID-5 vignette for the ranking demonstration over the HiTOP-BR vignette, because the full form's 25 facets make a top-5 ranking illustrative where 8 HiTOP-BR scales do not; falsified by the demonstration needing an argument the full form cannot show.
- 2026-08-30: plan gate chose shipping the sweep as a permanent test over a one-time check, because a one-time check reopens the gap on the next export; falsified by the test failing on an export deliberately left undocumented for a reason the deprecation exemption does not cover.
- 2026-08-30: plan gate chose adding the HiTOP-HSUM generator card over exempting that article, because the page already offers the prebuilt files those generators produce; falsified by a reader taking the card as a claim that HiTOP-HSUM scoring is supported.
- 2026-08-30: implement gate chose ranking the 25 facet columns over all 30 scored columns, so a domain built from facets cannot outrank them, and chose demonstrating both label targets (item wording, then scale name) over the help page's items-only example.
- 2026-08-30: T1 written tests-first and red on exactly the five real gaps (`generate_docx_hitophsum`, `generate_redcap_hitophsum`, `label_hitopbr`, `label_hitopsr`, `rank_scales`); the fixture arms pass 14 assertions, so the classifier's silence on `covered`/`linked` and its report on the prose, comment, `eval = FALSE` header, `#| eval: false` body, and absent arms are all shown.
- 2026-08-30: T3 added a `## Rank Each Person's Highest Facets` section to the full-form PID-5 vignette ranking the 25 facet columns with `top = 5`; the sweep no longer names `rank_scales`, leaving four gaps. Boxes stay unticked until the suite is green, since the sweep is red by design until T5 lands.
- 2026-08-30: T4 added a `## Labelling Columns` section to each of the HiTOP-SR and HiTOP-BR scoring vignettes, showing the item text on a raw item column and then the printed scale name on a scored column; run against `sim_hitopsr`/`sim_hitopbr` because `ku_hitopsr`'s zero-padded `hsr001` names match no prefix the labeller builds. The sweep is now down to the two HiTOP-HSUM generators.
- 2026-08-30: T5 added an "Explore the R Package Features" block to the HiTOP-HSUM download article carrying one Custom File Generation card, linking the reference pages of `generate_docx_hitophsum()` and `generate_redcap_hitophsum()` — the only two generators that instrument exports; the card's text says the Qualtrics file is a prebuilt download only, and the section's lead says scoring is still under development. The sweep is green.
- 2026-08-30: T2 planted each arm in the real files and recorded the red — the `rank_scales()` call demoted to a comment with the prose mention left standing reported `rank_scales`; the same call inside an `eval = FALSE` chunk reported `rank_scales`; deleting the two HiTOP-HSUM card links reported `generate_docx_hitophsum` and `generate_redcap_hitophsum`. Every other export stayed unreported in all three reds. The passing run asserts 39 exports and 15 `.Rmd` files, that the file list reaches `articles/`, and that `hitop_subset` is in the deprecation-exempt set.
- 2026-08-30: T6 added a NEWS "Documentation and website" section for the three vignette sections, the HiTOP-HSUM generator card, and the sweep. `document()` no diff, `devtools::test()` FAIL 0 / WARN 0 / SKIP 4 / PASS 16206, `devtools::check()` 0 errors / 0 warnings / 0 notes (8m 48s, vignettes rebuilt), `pkgdown::check_pkgdown()` no problems found. T1-T6 ticked; status to review.
- 2026-08-30: review ran the three-lens fan-out; the maintainer chose at the merge gate to fix findings 1-6 on the branch and file 7-12 as a candidate row. The `eval=F` arm added for finding 3 was shown red against the old regex before the fix was restored. Correcting the T2 line above: the passing run's floors were `>= 30` exports and `>= 10` files, not the 39 and 15 that line claims; finding 5's fix raises them to 39 and 15.

## Decisions

## Review

### Acceptance-criterion evidence (2026-08-30)

- AC1 — `Rscript -e 'devtools::test()'` ran the sweep green: 15 assertions in
  `test-vignette-export-coverage.R`, whole suite FAIL 0 / WARN 0 with the same 4
  pre-existing skips as the merge base. Counted at review, the sweep's two
  enumerations are 39 `export()` entries in `NAMESPACE` and 15 `.Rmd` files
  under `vignettes/` (5 top-level, 10 under `articles/`); it reports no
  uncovered export.
- AC2 — `vignettes/pid5_scoring.Rmd:62-72` carries `## Rank Each Person's
  Highest Facets`: an evaluated `{r rank}` chunk (no `eval` option) ranking the
  columns of `score_pid5(sim_pid5, items = 1:220, append = FALSE)`, the same
  scoring the vignette performs at line 34, and prose naming `top_scales` as
  holding each participant's five highest-scoring facets as one comma-separated
  string. Chunk re-run at review: returns a 100 x 31 tibble. Recorded against
  it: finding 1 below, that the ranked column is truncated out of the tibble the
  chunk prints.
- AC3 — `vignettes/hitopsr_scoring.Rmd:245-265` and
  `vignettes/hitopbr_scoring.Rmd:186-206` each carry a `## Labelling Columns`
  section with two evaluated chunks, each printing `attr(<column>, "label")` for
  a column named in the call: `hsr_1` / `hsr_agoraphobia` and `hitopbr_1` /
  `hbr_antagonism`. `devtools::check()`'s vignette re-build executed both files
  without error, so the demonstrated labels resolve.
- AC4 — `NAMESPACE` exports exactly two HiTOP-HSUM generators,
  `generate_docx_hitophsum` and `generate_redcap_hitophsum`. The card at
  `vignettes/articles/download-hitophsum.Rmd:60-61` links
  `../reference/generate_docx_hitophsum.html` and
  `../reference/generate_redcap_hitophsum.html`; both have an `Rd` page in
  `man/`, so pkgdown emits the pages those hrefs name. Recorded against it:
  finding 2 below, on the section's lead sentence.
- AC5 — run fresh at review on this branch: `devtools::test()` FAIL 0 / WARN 0
  with 4 pre-existing skips; `devtools::check(document = TRUE)` `Status: OK`
  (0 errors / 0 warnings / 0 notes), including the vignette re-build;
  `git status` after that run shows no change under `man/` or `NAMESPACE`, so
  `document()` produced no diff; `pkgdown::check_pkgdown()` "No problems found."

### Consistency gate (2026-08-30)

- `cairn_validate.py` exit 0, every check PASS. Advisories only: 21 dangling
  decision-id tokens (all pre-migration references carried by `DESIGN.md`,
  `DECISIONS.md` and `SOURCES.md`, pre-existing) and one references-staleness
  note on `schmukle2026.md` (pre-existing). The `release window` advisory did
  not fire.
- `cairn_impact.py` skipped: the diff changes no `DESIGN.md` principle.
- Toolchain checks from the `r-package` profile's `consistency-gate` slot:
  `document()` no diff, no hand-edit to `NAMESPACE`/`man/`/`data/*.rda`,
  `README.Rmd` and `README.md` both untouched by the branch,
  `check_pkgdown()` passes, `NEWS.md` carries a "Documentation and website"
  section under the development heading for the three vignette sections and the
  HiTOP-HSUM card, no new top-level files so no `.Rbuildignore` entry is owed,
  and `check()` is clean.

### Independent fresh-context review (2026-08-30)

Three reviewers, none having seen the implementation, each on a distinct
evidence base: [O] diff-bug over `git diff origin/main..HEAD`, [S]
blame-history over `git log`/`git blame` on the modified lines, [S]
prior-review-record over `cairn/milestones/archive/` plus a probe of the
GitHub inline-comment surface.

[S] blame-history: no findings. It reports that `calc_se` is a scoring-function
argument rather than an export, so the new sweep cannot touch it; that the new
vignette sections sit after the prose `test-vignette-se-prose.R` locks; that the
HSUM card omits a Qualtrics button because no such generator is exported; and
that the deprecation arm matches only `hitop_subset`.

[S] prior-review-record: the GitHub inline-comment surface is empty
(`gh api .../pulls/comments` returned `[]`), so the archive is the only record.
It reports no confirmed regression and two pattern-echoes it declined to call
hits — M044's finding on unverified vignette prose in `hitopsr_scoring.Rmd`, and
M039's `info =` labelling of assertions. The first was checked at review: the
new prose's two behavioural claims hold against `R/label_hitopsr.R` (the
no-match warning at line 38 and 55, the scale name from `hitopsr_scales$Scale`
at line 62). The second is a style point on an unmodified convention.

[O] diff-bug returned fourteen ranked findings. Verified at review before
triage, against the implementation rather than the reviewer's account:

1. `vignettes/pid5_scoring.Rmd:70` — the `rank_scales()` call uses the default
   `append = TRUE`, so the printed tibble is 100 x 31 and `top_scales` falls
   into the `27 more variables` truncation. Reproduced at review: the reader
   sees four facet score columns and never the column the section is about.
   AC2 as written is still met; the demonstration is not.
2. `vignettes/articles/download-hitophsum.Rmd:50` — the lead says the package
   "can already build the distributed files above from scratch", but the three
   cards above include a Qualtrics file the package exports no generator for.
   The card body two lines down states the exception, so the paragraph
   contradicts itself. Confirmed against `NAMESPACE`.
3. `tests/testthat/test-vignette-export-coverage.R:42` — the unevaluated-chunk
   detection matches the literal `eval = FALSE` only; `eval=F` is not caught.
   The reviewer also notes `#| eval: !expr ...` chunks count as evaluated, which
   is correct behaviour: those chunks do run when the condition holds.
4. `tests/testthat/test-vignette-export-coverage.R:30,38` — `is_text` is
   computed and then discarded, and the comment above it states an invariant
   ("a skipped chunk must not slip back in as a prose mention") that the
   returned `all = lines` does not enforce. AC1 asks only that the export be
   "linked there", with no chunk restriction, so behaviour matches the
   criterion; the dead variable and the false comment do not.
5. `tests/testthat/test-vignette-export-coverage.R:179-180` — the
   non-emptiness floors are `>= 30` exports and `>= 10` files while the actual
   counts are 39 and 15, and nothing asserts the glob still reaches the
   top-level `vignettes/` directory. The T2 work-log line's "asserts 39 exports
   and 15 `.Rmd` files" overstates what the floors bind.
6. `tests/testthat/test-vignette-export-coverage.R:182` — `get0()` is called
   without `inherits = FALSE`, so lookup walks the namespace's parents.
7. `tests/testthat/test-vignette-export-coverage.R:186-189` — the deprecation
   exemption matches any export whose deparsed body calls a `deprecate_*()`
   helper, so an argument-level deprecation inlined into a scoring function
   would exempt that whole function from coverage.
8. `tests/testthat/test-vignette-export-coverage.R:164-167` — the sweep skips
   when `vignettes/` is absent, which is the case under `R CMD check`, so CI
   never runs it; it fires only in a local `devtools::test()`.
9. `tests/testthat/test-vignette-export-coverage.R:51,58` — text-level parsing
   credits a call inside a string literal and truncates a line at a `#` that
   sits inside a string. No current vignette line has either shape.
10. `tests/testthat/test-vignette-export-coverage.R:34` — a chunk with no
    closing fence is dropped whole, and a four-backtick verbatim block counts
    as evaluated code.
11. `tests/testthat/test-vignette-export-coverage.R:63-66` — `linked_in()` does
    not check that the reference page it matches exists, so a link to a page
    pkgdown does not emit would still count as coverage.
12. `vignettes/pid5_scoring.Rmd:70` — `scales = 1:25` couples the demonstration
    to `score_pid5()`'s column order, which the prose asserts and no test pins.
13. `cairn/milestones/M073-export-vignette-coverage.md` — the AC checkboxes were
    unticked at the time the reviewer read the file.
14. Nits: British "Labelling" against the package's US "labeled"; the two label
    sections are near-verbatim duplicates; the fixture comparison at line 158
    depends on `sort()`'s locale.

### Triage and disposition (2026-08-30)

At the merge gate the maintainer chose to fix findings 1-6 on the branch and
file 7-12 as a candidate row.

- 1 — fixed. `vignettes/pid5_scoring.Rmd:70` passes `append = FALSE`, so the
  chunk prints the `top_scales` column itself; the prose below now says which
  argument produced that shape and what the default returns instead.
- 2 — fixed. The lead at `vignettes/articles/download-hitophsum.Rmd:51` now
  names the printable and REDCap files rather than all three downloads.
- 3 — fixed. The chunk-header test is `eval[ \t]*=[ \t]*F(ALSE)?[ \t]*[,}]`.
  A fixture arm calling `short_off(x)` in an `{r, eval=F}` chunk was added and
  shown red against the old regex (2 failures, including the whole-fixture
  identity assertion) before the fix was restored.
- 4 — fixed. The dead `is_text` vector is gone and the comment now states what
  `all = lines` actually means: the link arm is file-wide, only the call arm is
  chunk-restricted.
- 5 — fixed. The floors are `>= 39` exports and `>= 15` files, the counts the
  branch actually has, and a second assertion pins the glob reaching the
  top-level `vignettes/` directory as well as `articles/`.
- 6 — fixed. `get0()` takes `inherits = FALSE`.
- 7-12 — filed as one ROADMAP candidate row, lineage M073 (findings 7-12).
  None is a wrong verdict on the current vignettes; the row states its
  promotion condition as the sweep passing an export it should have named, or
  a vignette adding a line of one of the shapes the parser mishandles.
- 13 — rejected. The unticked boxes were review's own to tick against evidence,
  which this section does.
- 14 — rejected under the out-of-scope taxonomy: style nitpicks (British
  spelling, near-duplicate sections, `sort()` locale) on lines the milestone
  either introduced deliberately or did not modify.

Return floor: none of the fourteen demonstrates an acceptance criterion
failing, so the milestone did not return to `in-progress`.

### Re-verification after the gate fixes (2026-08-30)

- `devtools::test()` FAIL 0 / WARN 0, 4 pre-existing skips; the sweep file
  itself PASS 17 (up from 15, the two new `eval=F` assertions).
- `devtools::check(document = TRUE)` `Status: OK` -- 0 errors / 0 warnings /
  0 notes in 6m 13s, vignettes rebuilt, and a clean `git status` afterwards, so
  `document()` still produces no diff. `pkgdown::check_pkgdown()` "No problems
  found."
