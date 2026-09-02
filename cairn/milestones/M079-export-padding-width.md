<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M079: The online exports pad item numbers to the instrument's width, not to the export's own

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP2, GP3   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** `m079-export-padding-width` · https://github.com/jmgirard/hitop/pull/85   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

`build_qualtrics_txt()` and `build_redcap_zip()` zero-pad item numbers to the
width of the instrument's largest item number, stated by the wrapper that calls
them, rather than to the width of whichever items that one call exports.

## Scope
<!-- owner: plan · create/amend-via-gate -->

Surface tier: **internal** — no external consumer's output changes. Both
builders derive their padding width from the items handed to them
(`R/generate_qualtrics.R:287`, `R/generate_redcap.R:289`), so a HiTOP-SR module
whose items were all below 100 would be exported as `hsr_07`, which
`label_hitopsr()` cannot match. That module cannot be built from today's
keying: measured 2026-09-01 over all 76 rows of `hitopsr_scales`, the smallest
per-scale largest item number is 151 (`difficultiesReachingOrgasm`), against
`max(hitopsr_items$HSR)` of 405, so every module is already three digits wide.
The deliverable is therefore where the two builders get the width and the tests
that hold it there — no shipped or generated file moves.

**In:** a required instrument-width argument on both builders, passed by all ten
`generate_{qualtrics,redcap}_*` wrappers from the instrument's own item table;
the Qualtrics builder's inline width computation replaced by `item_names()`
(the M077 review's finding 5); tests for the width source, the required
argument, and a rebuild of every shipped online export against the committed
copy; the padding comments in `R/util.R:583-586` and both builders; the two
HiTOP-SR online generators' help pages stating that a module export keeps the
full instrument's width.

**Out:** renumbering module exports (excluded deliberately — D-036) · a
changelog entry, no user-visible output having changed (plan gate, 2026-09-01)
· `module` support for the HiTOP-BR and PID-5 generators → the "Generalize
modularization to BR/PID-5" candidate row · a descriptor recording the column
names an export produces → the descriptor-format candidate row · PID-5 dataset
item-column naming → its own candidate row.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [x] AC1: `build_qualtrics_txt()` and `build_redcap_zip()` pad to the
      instrument width they are given, not to the items they are given: called
      directly with an items frame whose largest item number is 45 and an
      instrument width of 405, they write the question id `HSR_007` and the
      variable name `hsr_007` for item 7.
- [x] AC2: Both builders require that width — a direct call omitting it aborts,
      rather than falling back to the exported items' own largest number.
- [ ] AC3: No shipped online export's item names move: for every row of
      `hitop_artifacts` whose `format` is `qualtrics` or `redcap` — the domain
      enumerated from the manifest — a fresh default build's item variable
      names are identical to those in the committed file the row names.
- [x] AC4: `Rscript -e 'devtools::test()'` clean and
      `Rscript -e 'devtools::document()'` producing no diff (the profile's
      `verify` slot).

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number. -->

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T4
- AC4 → T1, T2, T3, T4, T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: Give `build_qualtrics_txt()` (`R/generate_qualtrics.R:252`) and
      `build_redcap_zip()` (`R/generate_redcap.R:260`) a required instrument-width
      argument; build the Qualtrics ids through `item_names()` instead of the
      inline `nchar(as.character(max(items[[1]])))` at `R/generate_qualtrics.R:287`;
      pass the width from all ten wrappers, read from the instrument's own item
      table (`max(hitopsr_items$HSR)`, `max(hitopbr_items$HBR)`, the PID-5
      version columns).
- [x] T2: Test AC1 and AC2 by direct call on both builders with a synthetic
      items frame. Prove the AC1 test discriminates: revert each builder to
      `max(items[[1]])`, record the red run in the work log, restore.
- [x] T3: End-to-end smoke — a multi-scale HiTOP-SR module through
      `generate_qualtrics_hitopsr()` and `generate_redcap_hitopsr()` names its
      items exactly `item_names(prefix, <the module's numbers>, max_n =
      max(hitopsr_items$HSR))`.
- [x] T4: Test AC3 — iterate the `qualtrics`/`redcap` rows of
      `hitop_artifacts`, rebuild each with its generator's defaults into a temp
      file, and compare item variable names against the committed
      `inst/extdata/` file. Parse inside the REDCap container (zip builds are
      not byte-deterministic) and read the expected names off the committed
      artifact, never off the fresh build.
- [x] T5: Correct the padding comments (`R/util.R:583-586` says the width is
      "the export's for the generators") and both builders' comments; add to
      `generate_qualtrics_hitopsr()` and `generate_redcap_hitopsr()` help that a
      module export keeps the full instrument's item-name width; `document()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-09-01: created by /milestone-plan.
- 2026-09-01: criteria audit ran in reduced mode ([O], fresh context; internal tier) and returned a finding on all five drafted criteria — an unreachability floor and a comment-prose promise (both instrument properties), a mutation-discrimination clause (instrument), a merge-base rebuild comparison and a 76-module per-rendering enumeration (both disproportionate to the tier); each had one clear answer and was fixed before writing, leaving four criteria.
- 2026-09-01: plan gate chose an instrument-width argument on the two builders over deriving the width inside `apply_module()` because the five PID-5 wrappers of each family never call `apply_module()` and would keep a second width source; falsified by a wrapper family that cannot name its instrument's item table.
- 2026-09-01: plan gate chose comparing rebuilt exports' item names over comparing whole files because the committed artifacts carry build stamps unrelated to naming; falsified by a naming drift that leaves the item names equal while changing the rest of the file.
- 2026-09-01: plan gate chose fixing the latent defect over closing or narrowing the candidate row (Jeff, at the gate); falsified by the fix proving larger than the ten call sites and three tests scoped here.
- 2026-09-01: implement gate chose passing the instrument's largest item number over passing a digit count, because `item_names()` already derives the width from a largest number and a count would be recomputed in each of the ten wrappers; falsified by a caller that knows its width in digits but not its largest item number.
- 2026-09-01: implement gate chose `rlang::check_required()` over R's own missing-argument error for AC2, because it fires before any build work and names the argument in an assertable message; falsified by a caller relying on lazily-supplied `max_n`.
- 2026-09-01: implement gate chose covering all eleven generator-backed manifest exports in the AC3 rebuild test over covering only the nine numbered ones, pinning `hitophsum_qualtrics.qsf` by name as the sole file this package does not build; falsified by that .qsf gaining a generator.
- 2026-09-01: T1 done - both builders take a required `max_n`, the Qualtrics ids now come from `item_names()` rather than an inline `sprintf` format, and all ten wrappers pass their instrument's own table maximum (405, 45, 220, 100, 25).
- 2026-09-01: T2 done - AC1/AC2 tested by direct call on both builders with HiTOP-SR items 7 and 45. Discrimination proved: with each builder reverted to `max(items[[1]])` the two AC1 tests went red as `HSR_07`/`hsr_07` against the expected `HSR_007`/`hsr_007`, the other 36 assertions staying green; restored.
- 2026-09-01: T3 done - the three narrowest HiTOP-SR scales built through both online generators name items at the instrument's width; recorded in the test that this is a pass-through smoke check, not a discriminator, since the narrowest buildable module is still three digits.
- 2026-09-01: T4 done - the AC3 test enumerates the manifest's qualtrics/redcap files, derives each generator from the file name, asserts the one file without a generator is `hitophsum_qualtrics.qsf`, and compares eleven fresh default builds against the committed artifacts, parsing inside the REDCap zips. Proved able to fail: multiplying the HiTOP-BR Qualtrics width by ten turned it red naming `hitopbr_qualtrics.txt`; restored.
- 2026-09-01: T5 done - the `item_names()` comment no longer says the generators pad to the export's own width, both builders' padding comments state the instrument-width rule, and the two HiTOP-SR online generators' `module` help says item 4 stays `HSR_004`/`hsr_004`; `document()` rewrote those two .Rd files and a second run added nothing.
- 2026-09-01: `devtools::test()` clean (7 pre-existing skips: three merge-base guards, three keying-diff guards, OQ-1).
- 2026-09-01: the AC3 test compares fresh builds against the committed `inst/extdata/` artifacts, which `cairn/DESIGN.md`'s generator-testing decision says are not used as an oracle; the test comment states the distinction (a no-regression lock over names, not a content oracle) and leaves the DESIGN wording for review to rule on.
- 2026-09-01: all tasks done; `devtools::test()` clean, `devtools::document()` idempotent, `R CMD check` 0 errors / 0 warnings / 0 notes on the final tree; status to review.
- 2026-09-01: amendment return: AC3 — "for every row of `hitop_artifacts` whose `format` is `qualtrics` or `redcap`" — the manifest names `hitophsum_qualtrics.qsf`, which this package ships but has no generator for, so no fresh default build exists to compare; AC1, AC2 and AC4 verified, consistency gate green, review stops for the amendment alone.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->

2026-09-01. Evidence gathered fresh on the branch at commit 94db275a; `R CMD
check` and `devtools::test()` run on the working tree with `document()` clean.

**Sync.** `git fetch`; `origin/main` at 68ce4ae7, the branch 0 behind / 3
ahead — no merge needed. Branch pushed; draft PR
https://github.com/jmgirard/hitop/pull/85 opened.

### Acceptance criteria

- **AC1 — verified.** Direct call on both builders with an items frame holding
  HiTOP-SR items 7 and 45 (largest 45) and `max_n = 405`:
  `build_qualtrics_txt()` wrote the question ids `HSR_007`, `HSR_045`;
  `build_redcap_zip()` wrote the variable names `hsr_instructions`, `hsr_007`,
  `hsr_045`. The committed tests assert the same two identities.
- **AC2 — verified.** The same two direct calls with `max_n` omitted each
  aborted with an `rlang_error` reading "`max_n` is absent but must be
  supplied"; neither fell back to the exported items' largest number.
- **AC3 — not verified as written.** The criterion's domain is every
  `qualtrics`/`redcap` row of `hitop_artifacts`; that manifest names 12
  distinct files, and `generate_qualtrics_hitophsum()` does not exist, so
  `hitophsum_qualtrics.qsf` admits no fresh default build to compare. The
  implementation covers the other 11 and asserts by name that the .qsf is the
  sole file this package does not build — the implement gate's logged choice —
  but that narrowing was never made in the criterion's text. All 11 rebuilds
  matched their committed artifacts. See the amendment return below.
- **AC4 — verified.** `Rscript -e 'devtools::test()'` →
  `FAIL 0 | WARN 0 | SKIP 7 | PASS 16423` (the 7 skips are the pre-existing
  merge-base, keying-diff and OQ-1 guards). `Rscript -e 'devtools::document()'`
  left the tree unchanged apart from this milestone file.

### Consistency gate

- `cairn_validate.py` exit 0, all checks PASS; 22 advisories, all pre-existing
  (20 dangling `D-00x` tokens, 1 references-staleness, none introduced here).
- No `DESIGN.md` principle changed → `cairn_impact.py` not run.
- Profile `consistency-gate`: `document()` no diff; `NAMESPACE`/`man/`/`data/`
  regenerate clean; README untouched by the branch; `pkgdown::check_pkgdown()`
  "No problems found"; no NEWS entry, matching the plan gate's internal-tier
  choice; no new top-level files; `devtools::check()` 0 errors / 0 warnings /
  0 notes; line-ending policy check passed.

### Independent review (three lenses, fresh context)

- **[O] diff-bug.** No correctness defect. Verified all ten wrappers pass the
  right width (405, 45, 220, 100, 25), no other call site of either builder in
  `R/`, `tests/`, `data-raw/`, `devel/` or the builder repo, and the roxygen
  additions match observed behavior. Five ranked findings:
  1. The AC3 test reads its expectation off the committed `inst/extdata/`
     artifact, which `cairn/DESIGN.md`'s generator-testing decision says is not
     used as an oracle; the carve-out is written only in a test comment.
  2. `max_n` is guarded for shape (`min = 1`) but not for coherence with the
     items exported, so `max_n = 1` against items numbered to 405 would produce
     the unpadded names this milestone closes — internal-only, unreachable
     today.
  3. Three pre-existing test comments in `test-generate_qualtrics.R` and
     `test-generate_redcap.R` still state the old "widest exported item" rule;
     T5's comment sweep did not reach them.
  4. The AC3 test resolves generators with bare `exists()`/`get()`, which
     inherit from the search path; `envir = getNamespace("hitop")` would pin
     them.
  5. `manifest_generator()` assumes a `<stem>_<format>.<ext>` file-name grammar
     and hardcodes the census `sum(has_generator) == 11L`; a mis-mapped name is
     caught, but by the .qsf assertion rather than by a naming assertion.
- **[S] blame-history.** No conflict with D-036 (the change alters padding
  width only, never which number names which item) and none with the M077
  decision this fix was deferred from. No resurrected bug: this is the
  defect's first fix. Raised the same DESIGN tension as [O] finding 1, and
  found it pre-existing — several M046/M048-era tests already compare against
  `inst/extdata/`.
- **[S] prior-PR-comments.** No prior-review regression. The M077 review's
  finding 5 (the Qualtrics generator computing its width inline) is what this
  diff resolves; the M050 `validate_count()` guard pattern is followed; the
  M024, M075 and remaining M077 findings all concern lines this diff does not
  touch.

Findings 1–5 are logged here and carried to the next review gate for triage;
none of them demonstrates an acceptance criterion failing, and none is a
load-bearing defect in what the package does for its users, so none returns
the milestone on its own.

### Disposition — amendment return on AC3

AC3 promises the rebuild comparison over every `qualtrics`/`redcap` manifest
row, and one such row names a file this package ships but does not generate.
The work is right; the criterion's domain is wider than any implementation
could satisfy. Under the never-reinterpret rule this is the criterion's defect,
not the work's: it routes to the gated criterion-amendment protocol
(`/milestone-implement` step 6) to narrow AC3's domain to the manifest's
generator-backed rows, with `hitophsum_qualtrics.qsf` excluded by name as the
test already asserts. No other work is convened by this return.
