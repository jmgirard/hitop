# M055: The HiTOP-SR generators write a descriptor beside the file they build

- **Status:** review
- **Priority:** normal
- **Depends on:** M054
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP3
- **Branch/PR:** `m055-generator-descriptor-sidecar` / https://github.com/jmgirard/hitop/pull/61

## Goal

Let one generator call produce both the file a researcher fields and the
descriptor that scores the data it comes back as, including the printed order
of a shuffled Word form.

## Scope

Surface tier: **user-facing** — a new argument on three exported generators and
a file researchers keep.

**In:** a `descriptor` argument on `generate_docx_hitopsr()`,
`generate_qualtrics_hitopsr()`, and `generate_redcap_hitopsr()` writing M054's
format to the given path; the descriptor of a `module = NULL` call naming every
scale, so a full administration is described too; `itemOrder` populated from
the `item_order` the Word generator already returns
(`R/generate_docx.R:311`), which gives a shuffled form the record DESIGN known
issue 8 says the researcher is currently told to keep and not given; help
pages, vignette, NEWS.

**Out:** the browser builder offering the descriptor as a download → M056
(planned, depends on this), which closes the app half of known issue 8.
Recording the export's own column names (`HSR_007`, `hsr_007`) so scoring can
resolve `items` → the candidate row M054 opened. HiTOP-BR and PID-5 generators
→ the standing modularization-generalization row; they take no `module`.

## Acceptance criteria

- [x] AC1 For each of the three named HiTOP-SR generators, a call passing a
      four-scale `module` and a `descriptor` path writes a file whose
      `format`, `instrument`, `scales`, and `items` fields, parsed with
      `jsonlite::fromJSON()` rather than through `read_module()`, equal values
      derived directly from `hitopsr_scales` and `hitopsr_items`, and which
      `read_module()` returns a module `identical()` to that `module` from —
      one test per generator, all three named in the test.
- [x] AC2 For each of the same three generators, a call passing `descriptor`
      and no `module` writes a descriptor naming exactly the `Scale` column of
      `available_scales("hitopsr")`, and `score_hitopsr()` given that
      descriptor's module over all 405 `sim_hitopsr` item columns returns a
      tibble `identical()` to the same call passing no `module`.
- [x] AC3 The descriptor's `itemOrder` records original HSR item numbers. For
      `generate_docx_hitopsr(randomize = TRUE, descriptor = )` under a seed
      fixed in the test, run once with a module and once without and at each of
      `renumber = TRUE` and `renumber = FALSE`, scoring responses laid out in
      printed order after reordering by that `itemOrder` returns a tibble
      `identical()` to scoring the same responses laid out in instrument order
      — the expected side read off the responses, never back through the
      mapping under test (the M046 lesson).
- [x] AC4 With `randomize = FALSE`, and for the two online generators, which
      never shuffle, the written descriptor carries no `itemOrder`.
- [x] AC5 For each of the three generators, a `descriptor` that is not a
      length-one character string aborts with a classed `cli` error naming the
      argument, and an unwritable `descriptor` path aborts naming the path
      before any instrument file is written, asserted by checking that the
      `file` path does not exist after the abort.
- [x] AC6 The three help pages document `descriptor`,
      `vignettes/articles/modules-hitopsr.Rmd` shows the one-call form,
      `NEWS.md` records the argument, and DESIGN known issue 8 is rewritten to
      the app-only remainder M056 carries.
- [x] AC7 `devtools::document()` produces no diff, and `devtools::test()` and
      `devtools::check()` are clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T3
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T1, T3
- AC6 → T4
- AC7 → T4, T5

## Tasks

- [x] T1 Add `descriptor` to the three generators, resolving the full-instrument
      case to a module over every scale, and guard it beside the existing
      `file` checks.
- [x] T2 Thread `item_order` from `generate_docx_hitopsr()`'s existing
      computation (`R/generate_docx.R:217`, returned at `:311`) into the
      descriptor writer.
- [x] T3 Tests in `tests/testthat/test-module_file.R` or a sibling, covering
      AC1–AC5.
- [x] T4 Docs: the three help pages, the vignette, NEWS, and the DESIGN known
      issue 8 rewrite.
- [x] T5 Run the PROFILE verify slot.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan gate split the sidecar work from M054's format work so the format round-trips before three call sites depend on it; the alternative, one milestone doing both, was rejected at the gate and is recorded on M054.
- 2026-08-24: plan chose splitting the builder-page download into M056 rather than carrying it here, because the two live in different repos and the combined scope trips the sizing tripwires; falsified by the app change proving to be a few lines that would have been cheaper alongside these.
- 2026-08-24: fresh-context criteria audit ran in FULL mode (user-facing tier) and returned findings on AC1, AC3, and AC5; all three were repaired before this file was committed. AC1 checked the writer only through the package's own reader (IP2) — now the written fields are parsed independently and asserted against `hitopsr_scales`/`hitopsr_items`. AC3 left the shuffle seed and `renumber` unvaried, the axis where an `itemOrder` recorded in printed numbers rather than original HSR numbers would pass — now seeded and run at both `renumber` values, with the numbering stated. AC5 named an internal call site and left the abort ordering to the implementation, which constrains nothing — now behavioral, with the ordering fixed. AC2, AC4, AC6, and AC7 returned nothing.
- 2026-08-24: T1 — `descriptor` added to `generate_docx_hitopsr()`, `generate_qualtrics_hitopsr()`, and `generate_redcap_hitopsr()`, placed last before the deprecated `subset` so no positional call shifts; `module = NULL` resolves to a module over every scale, so a full administration is described too.
- 2026-08-24: implementation gate chose `write_module()` writing an `item_order` attribute as the file's `itemOrder` over an internal-only writer (recorded below as M055-D1), and chose removing an already-written sidecar when the instrument build then fails over leaving it behind.
- 2026-08-24: T2 — the Word generator threads its existing printed-order map into the sidecar under `randomize = TRUE` only; without a shuffle the module's ascending `items` already states the order.
- 2026-08-24: T3 — `tests/testthat/test-generator-descriptor.R` covers AC1–AC5, parsing the written file with `jsonlite::fromJSON()` against `hitopsr_items`/`hitopsr_scales` rather than through `read_module()`. Each new check was proven able to fail by planting the defect it claims to catch: printed numbers in `itemOrder`, a sorted `itemOrder`, an `itemOrder` written for an unshuffled form, and a sidecar left behind after a failed build.
- 2026-08-24: the first wiring passed each build to a shared `with_descriptor(build = function() ...)` helper; `test-export-arg-guards.R` reddened, showing the generators' own guards blaming `build` instead of the exported function, and the wrapper also made the two online generators return their path visibly. Replaced by an inline `on.exit()` in each generator; both regressions gone.
- 2026-08-24: T4 — the three help pages document `descriptor`, `vignettes/articles/modules-hitopsr.Rmd` shows the one-call form and the shuffled-form order it saves, NEWS records the argument, and DESIGN known issue 8 is narrowed to the app remainder M056 carries.
- 2026-08-24: T5 — `devtools::document()` produces no diff, `devtools::test()` is clean (1 pre-existing skip, the OQ-1 keying question), and `devtools::check()` returns 0 errors / 0 warnings / 0 notes. `pkgdown::build_article("articles/modules-hitopsr")` renders the edited article, which `R CMD check` does not cover.
- 2026-08-24: status set to `review`.
- 2026-08-24: review returned M055 to `in-progress` at the return floor. Finding 1 of the diff-bug lens falsifies AC4: `write_descriptor_sidecar()` never clears an `item_order` attribute the incoming module already carries, so a module read back from a shuffled Word form's descriptor makes `generate_qualtrics_hitopsr()`, `generate_redcap_hitopsr()`, and `generate_docx_hitopsr(randomize = FALSE)` write an `itemOrder` into a descriptor for a form that was never shuffled. Reproduced fresh. Findings 2, 3, 6, 7, and 8 ride the same return; 4 and 5 are the maintainer's call. Defect returns on this milestone: 1.
- 2026-08-24: return gate chose always removing the sidecar when the build fails, a file already at that path included (M055-D2), and chose an internal shared writer over adding a `call` argument to the exported `write_module()`.
- 2026-08-24: finding 1 (AC4 falsified) — `write_descriptor_sidecar()` now sets `item_order` unconditionally, so a NULL order clears one the incoming module already carried. Reproduced first: a module read back from a shuffled Word form's descriptor stamped `66 144 389 109 260 118 291 202` on a Qualtrics export. The regression test was proven red against the old line.
- 2026-08-24: findings 2, 3, 6, 7, 8 — the write body moved to an internal `write_module_impl(module, file, call)`, so an unwritable path now blames the generator called rather than `write_module(module, descriptor)`; rollback uses `file.remove()` on the literal path instead of `unlink()`, which glob-expanded a path holding `*`, `?` or `[`; the test file filters the generator loop by each generator's own dependencies rather than skipping all three when the Word stack is absent; AC3's returned-attribute check is labeled for what it checks; `generate_redcap_hitopsr()` gains the descriptor `@seealso`. Findings 2 and 3 were each reproduced and each new check proven red against the old code. Finding 4 is settled by M055-D2; finding 5 (promoting M055-D1) stays for the review gate.
- 2026-08-24: `devtools::document()` produces no diff, `devtools::test()` is clean (FAIL 0, WARN 0, SKIP 1 pre-existing, PASS 14391), and `devtools::check()` returns 0 errors / 0 warnings / 0 notes. Status set back to `review`.
- 2026-08-24: re-review — every acceptance criterion re-verified with fresh evidence on the repaired branch, AC4 included (its return scenario reproduced independently as well as in the suite). Gate checks clean. The three-lens fan-out re-ran: blame-history and prior-review lenses returned no findings; the diff-bug lens confirmed all six repairs and returned eight further findings, none falsifying a criterion, taken to the approval gate for triage.
- 2026-08-24: gate triage — findings 1 and 2 fixed on the branch: `write_module()` and the sidecar writer refuse an empty path, and a shared `validate_descriptor_target()` refuses a `descriptor` naming the same path as `file`, each guard blaming the argument the caller passed. Three regression tests, each proven red against the unguarded code. M055-D1 promoted to D-040. Suite PASS 14419, `check()` 0/0/0.

## Decisions

### M055-D1 (2026-08-24): `write_module()` writes a module's `item_order` attribute as the file's `itemOrder`

**Context:** The descriptor format already defines `itemOrder`, and `read_module()` already returns it on the module's `item_order` attribute, but M054 documented `write_module()` as never writing it — so a descriptor read and written again lost the printed order it recorded, and the generators' new `descriptor` argument would have needed a private writer to save one.
**Decision:** `write_module()` writes `itemOrder` whenever the module carries an `item_order` attribute, after checking that the attribute is a permutation of the module's items rather than trusting it. The generators set the attribute; nothing else about the format changes.
**Consequences:** Saving and reading a descriptor are symmetric. The field's definition and the version string are untouched, so the format contract D-039 fixed is not reopened; one paragraph of `?write_module` is rewritten, and a hand-set attribute that is not a permutation is now an error at write time instead of at read time.

### M055-D2 (2026-08-24): a failed build removes the descriptor even when a file was already at that path

**Context:** The generators write the descriptor before the instrument file and remove it again when the build fails, so no descriptor is left describing a form that was never written. The review asked what should happen when the path already held a file: the write has already replaced its contents by then, so the old descriptor is gone either way.
**Decision:** The cleanup always removes the descriptor path the call was given, whether or not a file was there before it. The removal names the literal path, so a `descriptor` holding `*`, `?` or `[` deletes only itself.
**Consequences:** A caller who points `descriptor` at an existing file and whose build then fails is left with no file at that path. The three generator help pages say so.

## Review

### Acceptance criteria — fresh evidence (2026-08-24, second pass)

Branch synced: `main` == `origin/main` and contained in `HEAD`; nothing to
merge, so every result below is from the current branch at `db4ab7f`. All
evidence is re-run after the repair commit; the first pass's lines are
superseded.

- AC1 — `tests/testthat/test-generator-descriptor.R` "…write a module's
  descriptor beside the file" runs the four-scale module through all three
  named generators; 19 expectations pass, none skipped. The written file is
  parsed with `jsonlite::fromJSON()` and its `format`, `instrument`, `scales`,
  `items`, and `nItems` compared against values derived from `hitopsr_items`
  and `hitopsr_scales`; `read_module()` then returns a module `identical()` to
  the one passed.
- AC2 — "a generator call with no module writes a descriptor of the whole
  instrument": 6 expectations pass. The descriptor's `scales` equals the
  `Scale` column of `available_scales("hitopsr")` for each generator, and
  `score_hitopsr()` over all 405 `sim_hitopsr` columns through that module is
  `identical()` to the same call with no `module`.
- AC3 — "a shuffled Word form's descriptor records the printed order in
  original HiTOP-SR numbers": 18 expectations pass across the four cases
  (module / whole instrument × `renumber = TRUE` / `FALSE`) under
  `set.seed(20260824)`. `itemOrder` is asserted to be a permutation of the
  covered original numbers and not the identity; the expected scores are read
  off the responses laid out in instrument order, never back through the
  mapping under test.
- AC4 — "a descriptor for an unshuffled form carries no printed order": 6
  expectations pass, and the return's regression test "a module that already
  carries a printed order does not stamp one on a form that was never
  shuffled" adds 6 more. Reproduced independently of the suite as well: a
  module read back from a shuffled Word form's descriptor (its `item_order`
  present on read) driven through `generate_qualtrics_hitopsr()`,
  `generate_redcap_hitopsr()`, and `generate_docx_hitopsr(randomize = FALSE)`
  writes no `itemOrder` into the parsed JSON and returns no `item_order` on
  read, in all three cases — the exact scenario the first pass returned on.
- AC5 — "every generator refuses a descriptor that is not a single path" (18
  expectations) and "an unwritable descriptor path is refused before any
  instrument file is written" (9). Each abort is an `rlang_error` whose message
  names `descriptor`, or names the path; the instrument `file` is asserted not
  to exist after the abort. Three sibling tests add 21 more: no sidecar left
  behind when the build fails, the refusal blaming the generator called, and
  rollback removing only the literal path.
- AC6 — `descriptor` is documented in all three help pages
  (`man/generate_docx_hitopsr.Rd:78`, `man/generate_qualtrics_hitopsr.Rd:42`,
  `man/generate_redcap_hitopsr.Rd:38`), each now carrying the `@seealso` line;
  `vignettes/articles/modules-hitopsr.Rmd` shows the one-call form and reads
  the printed order back (`:271`–`:305`); `NEWS.md:31`–`:46` records the
  argument with no milestone numbers in user-facing text; `cairn/DESIGN.md:119`
  known issue 8 is rewritten to the app-only remainder M056 carries.
- AC7 — `devtools::document()` re-run leaves the working tree clean.
  `devtools::test()` over the whole suite: FAIL 0, WARN 0, SKIP 1 (the
  pre-existing OQ-1 keying skip at `test-keying.R:102`), PASS 14391.
  `devtools::check()`: Status OK — 0 errors, 0 warnings, 0 notes (3m 49.5s).
  Re-run after the gate fixes below: `document()` no diff, `devtools::test()`
  FAIL 0 / WARN 0 / SKIP 1 / PASS 14419, `devtools::check()` Status OK — 0
  errors, 0 warnings, 0 notes (4m 40s).

### Consistency gate (2026-08-24, second pass)

Universal cairn-file checks: `cairn_validate.py` exits 0, "all checks passed",
with 21 advisory dangling-id warnings, all pre-existing references to the
pre-migration D-001–D-012 entries; the `release window` advisory did not fire.
No `DESIGN.md` principle (IP/GP) text changed — only known issue 8 — so
`cairn_impact.py --changed` does not apply.

Toolchain checks from the `r-package` profile's `consistency-gate` slot:
`document()` no diff; no hand-edited generated files (the no-diff run covers
it); README.Rmd untouched, so README.md is in sync; `pkgdown::check_pkgdown()`
reports no problems; NEWS.md carries the user-visible entry with no milestone
numbers; no new top-level files, so no `.Rbuildignore` entry is owed;
`devtools::check()` clean as recorded under AC7.
`pkgdown::build_article("articles/modules-hitopsr")` renders the edited
article, which `R CMD check` does not cover.

### Independent fresh-context review (2026-08-24, second pass)

Surface tier user-facing and the diff touches `R/`, so the full three-lens
fan-out ran again on the repaired branch: an Opus diff-bug lens, a Sonnet
blame-history lens, and a Sonnet prior-review lens, each with its own evidence
base and none having seen the implementation.

Blame-history lens: no findings. M055-D1's reversal of M054's "never writes
`itemOrder`" clause is what D-039 itself scheduled, the
`write_module()`/`write_module_impl()` split follows the file's existing
`call =` blame convention, and no participant-facing content changed (IP1 is
not reopened).

Prior-review lens: no reintroduced or contradicted prior finding. The GitHub
inline-comment probe returned empty, so the per-PR walk was skipped; the
archived `## Review` sections and LESSONS entries on these files were checked
and found honored, and all six repaired findings from the first pass were
confirmed addressed rather than papered over. One sub-threshold observation:
`write_module_impl()`'s own `call = rlang::caller_env()` default is never
exercised, both call sites passing `call` explicitly.

Diff-bug lens: the six claimed repairs were each reproduced and hold — AC4's
attribute now clears, the refusal blames the generator, rollback is literal,
the test loop filters per dependency, the AC3 attribute check is labeled, and
the REDCap `@seealso` is present. Eight further findings, ranked, with
dispositions:

1. **`descriptor = ""` silently writes no descriptor and the call reports
   success.** `validate_string()` accepts `""`, and `writeLines(json, con =
   "")` opens an anonymous connection whose contents are discarded, its warning
   swallowed by the surrounding `suppressWarnings()`. Reproduced fresh:
   `generate_qualtrics_hitopsr(file = p, module = m, descriptor = "")` prints
   its success alert, writes the export, and leaves no descriptor anywhere.
   The write body is inherited from M054, but M055 is what makes it reachable
   from three exported user-facing arguments. Falsifies no criterion — AC5
   promises a refusal for a value that is "not a length-one character string",
   and `""` is one. **Fixed at the gate** (2026-08-24): `write_module()` and the
   sidecar writer both refuse an empty path, each naming the argument the
   caller passed, with regression tests proven red against the unguarded code.
2. **`descriptor` pointing at the same path as `file` is silently clobbered.**
   Reproduced fresh: the descriptor is written, the export then overwrites it,
   `built <- TRUE` suppresses rollback, and the call reports success; the file
   left on disk is the Qualtrics export and `read_module()` refuses it. Same
   silent-no-descriptor outcome as finding 1. No AC touched. **Fixed at the
   gate** (2026-08-24): a shared `validate_descriptor_target()` refuses the
   pair before either file is written, comparing the resolved directory and
   basename so `dir/./m.json` and `dir/m.json` are seen as one path.
3. **`cairn/DECISIONS.md` now misdescribes the shipped format.** D-039's
   entry still calls `itemOrder` a reserved slot `write_module()` never
   populates, while D-039(d) says the fields change "only through a further
   D-entry"; M055-D1 is that entry but lives only in this milestone file. Not
   a code defect — carried over from the first pass, where it was finding 5.
   **Promoted to `cairn/DECISIONS.md`** at the gate
   as D-040, annotating D-039.
4. **AC4's checkbox was unticked and its evidence line stale** when the repair
   commit set status back to `review`. **Resolved by this pass** — fresh
   evidence recorded above and the box re-ticked against it.
5. **The rollback block is triplicated verbatim** across the three generators.
   The work log records why the closure-taking wrapper was reverted (it broke
   argument blame); a narrower helper could keep both. Quality only.
   **Rejected at triage** — style, and the reverted alternative is on record.
6. **The descriptor's creation is never announced**, unlike every instrument
   file, which ends in a `cli_alert_success()` naming its path. No AC touched.
   **Follow-up** — a candidate line, written in the hygiene commit.
7. **`descriptor` sits ahead of the deprecated `subset` in all three
   signatures**, so the work log's "no positional call shifts" holds for every
   argument except `subset` itself, which an eight-argument positional call
   would now land in `descriptor`. **Rejected at triage** — `subset` is
   deprecated and warns, and the shift needs a fully positional eight-argument
   call.
8. **The reorder recipe is unqualified for `renumber = FALSE`.** Both
   `?generate_docx_hitopsr` and the vignette say to apply
   `collected[order(item_order)]`, which is right when the columns are in
   printed *position* order and double-permutes if a researcher keyed them by
   the printed original number instead. Wording inherited from main; M055's
   vignette repeats it. **Follow-up** — widens the existing
   `modules-hitopsr.Rmd` candidate row.

### Outcome

Every acceptance criterion is verified against fresh evidence recorded above,
AC4 included. Eight diff-bug findings were triaged at the gate: two silent
no-descriptor holes (findings 1 and 2) fixed on the branch with regression
tests proven red against the unguarded code, M055-D1 promoted to D-040, two
rejected as style or negligible, two routed to candidate rows, and one
(AC4's stale evidence) resolved by this pass. No finding met the return floor.
Defect returns on this milestone: 1.
