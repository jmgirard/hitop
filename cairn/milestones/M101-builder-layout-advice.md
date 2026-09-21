# M101: The builder tells users to score shuffled Word forms with `layout = "printed"`

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — researchers read the builder page, the bundle README.txt and the builder README
- **Branch/PR:** m101-builder-layout-advice; code https://github.com/jmgirard/hitop-builder/pull/17 (tracking PR in this repo)

## Goal

The HiTOP-SR Module Builder tells users of a shuffled Word form to score printed-order columns with `read_module()` and `layout = "printed"`. It no longer says that they must reorder the columns by hand.

## Scope

**In:** All code changes are in jmgirard/hitop-builder. This repo gets tracking only.

- The shuffle notice `#shuffleNote` (`index.html:540-543`) gives two routes. Columns in printed order take `read_module()` and `layout = "printed"`. Columns already in HiTOP-SR order take the default layout.
- `bundleReadme()` (`index.html:1200`) takes a shuffle flag. A shuffled Word bundle's `README.txt` adds one sentence that names `read_module()` and `layout = "printed"`. The call at `index.html:1349` passes the flag.
- `tests/prose.mjs` also extracts the shuffled Word README (`bundleReadmePassages()`, `tests/prose.mjs:364`).
- README.md §The scoring file and §Shuffling the Word form name `layout = "printed"`. The sentence that says the help page "states the same reordering rule" (README.md:368-369) now matches the help page.

**Out:**

- The download-button race stays a candidate row. This milestone changes one argument in `download()`, which is not the re-cut that row waits for.
- The `prose.mjs` writer-guard gaps (narrow grep, no npm script or CI step) stay a candidate row. This milestone adds no writer site.
- The package side is unchanged. M091 shipped `layout` and its help text.

## Acceptance criteria

- [x] AC1: With the shuffle box ticked, the page's shuffle notice names `read_module()` and `layout = "printed"` for columns in the order the form printed them. It says that columns already in HiTOP-SR order take the default layout. No sentence in the notice says that the columns must go back into instrument order before scoring. Evidence: a read of the notice in the served page on each of the four numbering and selection builds.
- [x] AC2: The `README.txt` in a shuffled Word bundle names `read_module()` and `layout = "printed"` for printed-order columns. The added sentence is the same for both numbering modes and both selections. Every unshuffled build's `README.txt` is byte-identical to the merge base's. Evidence: the page's own `bundleReadme()` run on the branch and on the merge base with one fixed version string, for eight builds, then a diff. The eight builds are three formats by two selections, plus the two shuffled Word selections.
- [x] AC3: In the builder README.md, §Shuffling the Word form and §The scoring file name `layout = "printed"`. The sentence about the help page's "same reordering rule" is replaced by one that matches the `randomize` paragraph of `generate_docx_hitopsr()` in hitop main.
- [x] AC4: The domain is the user-facing text of the builder, in three parts. The first is the served page on the four numbering and selection builds with the shuffle box ticked, which render the three `crosswalkSentence()` branches. The second is every bundle `README.txt`. The four that `tests/prose.mjs` lists, one per format and the shuffled Word one, stand for the rest. The rest differ from them only in the file stem and the hitop version, which appear only in file names and the header line. The third is README.md. Each passage in the domain that says how to score printed-order columns names `layout = "printed"`. Each one that says how to score columns in HiTOP-SR order names the default layout. Evidence: `tests/prose.mjs` lists the passages, and each one that mentions a shuffled form or a printed order is read.
- [x] AC5: The advice scores correctly on three shuffled Word bundles from the served page. They are a module numbered 1 to n, the whole instrument, and a module with the instrument's own numbers. For each, the printed order comes from the `.docx`, never from the `.json` under test. Printed-order columns scored with `module = read_module(<bundle .json>)` and `layout = "printed"` equal the same responses in HiTOP-SR order, scored with the same module and the default layout. Evidence: an R run against hitop main.
- [x] AC6: Every changed or added passage, bundle READMEs included, reports 0 findings under `ste_lint.py --type descriptive`. The builder smoke test passes locally and in the builder PR's CI.

## Coverage

- AC1 → T1, T6
- AC2 → T2, T4
- AC3 → T3
- AC4 → T1, T2, T3, T6
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1: In hitop-builder, on branch `m101-builder-layout-advice`, rewrite the first paragraph of `#shuffleNote` (`index.html:540-543`) to give the two routes. Read the four `crosswalkSentence()` branches (`index.html:1088-1103`). If a branch contradicts the notice, change that branch.
- [x] T2: Give `bundleReadme()` a `shuffle` argument and add the sentence for a shuffled Word bundle. Pass the flag at `index.html:1349`. Extend `bundleReadmePassages()` in `tests/prose.mjs` to extract the shuffled Word README. If a ledger anchor moves, update the writer ledger.
- [x] T3: Edit README.md §The scoring file (near line 162) and §Shuffling the Word form (lines 343-369). Read the `randomize` paragraph in hitop `R/generate_docx.R` first and match it.
- [x] T4: Run `bundleReadme()` from the branch and from the merge base for the eight builds with version `0.2.0`, and diff the outputs. Record the result in the work log.
- [x] T5: Serve the page and download the three shuffled Word bundles. Read each printed order off the `.docx` (crosswalk, printed number, or item text against `hitopsr_items`). Build responses in HiTOP-SR order, derive the printed-order columns from the `.docx` order, and compare the two scorings in R against hitop main. Pass `items` as positions, because names trip the order warning under `"printed"`. Record the builder commit and the result.
- [x] T6: Run `ste_lint.py --type descriptive` on the changed passages of the page, README.md and the bundle READMEs. Read the served notice on the four builds and every passage that AC4 names. Run the smoke test locally. The builder PR, its CI run, and the tracking PR open at `/milestone-review`.

## Work log

- 2026-09-21: created by /milestone-plan. Absorbs the candidate row on the builder README and bundle README.txt (lineage: M091 plan gate). That row said the bundle README.txt tells users to reorder by hand. Since M092 it does not mention reordering.
- 2026-09-21: criteria audit (full mode, fresh [O] reader) returned six findings, all fixed before the gate. AC1 now names the default layout for columns in HiTOP-SR order. AC2 uses one fixed version string. AC4 names its domain, with `prose.mjs` as the lister. AC5 adds an original-numbering bundle and names the module on both calls. AC6 no longer binds `prose.mjs` itself.
- 2026-09-21: AC5 reads the printed order off the `.docx`, per the M046 lesson that an oracle inverting the map under test asserts nothing.
- 2026-09-21: plan gate chose a shuffled-bundle README sentence over leaving README.txt alone because a bundle reader never sees the page; falsified by a report that the sentence confuses readers.
- 2026-09-21: plan gate chose three served bundles over M091's tests alone because no test scores a page-written descriptor; falsified by a run that finds nothing new.
- 2026-09-21: plan gate kept the download-button race separate over folding it in because one argument is not a re-cut; falsified by a bundle holding the wrong build.
- 2026-09-21: plan chose a two-route notice over a `"printed"`-only notice because `"printed"` scrambles HiTOP-SR-order columns; falsified by readers the two routes confuse.
- 2026-09-21: T1, T2 done in builder commit fb99dcb. The notice gives both routes. The four crosswalkSentence() branches are three returns (original numbering covers both selections), and none contradicts the notice, so none changed. bundleReadme() takes `shuffle = false` and adds one paragraph for a shuffled Word form. prose.mjs lists `readme:docx-shuffled`, ledger unchanged at 18 writer sites. ste_lint on all 154 passages: 0.
- 2026-09-21: T3 done in builder commit 8bdf7af. §The scoring file names score_hitopsr() and `layout = "printed"`, and the README-differences paragraph names the shuffled Word paragraph. §Shuffling the Word form gives both routes. The help-page sentence now points at the `randomize` paragraph of generate_docx_hitopsr() (hitop `R/generate_docx.R:121-131`), read this session. ste_lint: 0.
- 2026-09-21: T4 done. bundleReadme() extracted from builder main 1987f4a and branch 8bdf7af, version `0.2.0`, eight builds: the six unshuffled READMEs are byte-identical. Each shuffled Word README removes 0 lines and adds the same 4-line paragraph (plus its blank separator) under the scoring file, identical for every scale and some scales. Script in the session scratchpad, not committed.
- 2026-09-21: T5 done. A Playwright script drove the page served from builder branch 8bdf7af (webR, hitop 0.2.0 from r-universe) and saved three shuffled Word bundles: a two-scale module numbered 1 to n (13 items), the whole instrument numbered 1 to n (405), and the same module with original numbers. Printed order read from each .docx item table by matching item text to `hitopsr_items$Text`. R/ identical to hitop main 151de796, seed 101, 50 random respondents, items as positions. All three: `layout = "printed"` on printed columns equals the default layout on instrument-order columns (all.equal TRUE). Control: the default layout on printed columns differs in all three. The .docx order also equals each .json itemOrder. Scripts in the session scratchpad, not committed.
- 2026-09-21: T6 done at builder commit cff624d. ste_lint over all 154 passages from prose.mjs: 0. The served notice, read on the four numbering and selection builds (T5 script), names both routes on each. Of the 154 passages, 17 mention a shuffled form or a printed order. The four that tell the reader how to score printed-order columns (body `#shuffleNote`, readme:docx-shuffled, md:The scoring file, md:Shuffling the Word form) name `layout = "printed"`, and each names the default layout for HiTOP-SR-order columns. The HTML comment above `#shuffleNote` no longer says that the form is not scored as it stands. Smoke 1/1 locally (12.4s). Minor amendment: T6's PR-opening moves to `/milestone-review`, which opens the PRs and waits on CI.
- 2026-09-21: amendment (mini gate, Jeff chose amend): AC4 said "all four `crosswalkSentence()` branches" where the function has three returns.
- 2026-09-21: re-audit: AC4 (full) — three findings: "passage" unbounded against crosswalkSentence[0], the README domain wider than prose.mjs lists, the builds not named as shuffled. All three fixed.
- 2026-09-21: re-audit: AC4 (full) — two findings: bundleReadme() also takes the version, and prose.mjs splits the notice into several passages. Stop reached, fixes put to Jeff, who chose the final wording.
- 2026-09-21: AC4 amended to its final wording. The T6 evidence covers it: the four builds were read with the box ticked, and the four scoring passages name the right layout.
- 2026-09-21: claim audit: 16 claims read, 2 corrected — hitop-builder index.html, README.md. The notice's "nothing warns you" was wrong (warn_item_order() fires on printed-order original-number names under the default layout) and now says "raises no error". README now says score_hitopsr() reads item_order when it gets that module and `layout = "printed"`. The same reader re-read both as accurate. Builder commit c64ade9, smoke 1/1 after the fix.
- 2026-09-21: implement done. hitop verify: devtools::test() 0 failed, 0 errors, 13 skipped, 17564 passed (no R change on this branch). Status to review.
- 2026-09-21: step-7 approval: m101-builder-layout-advice approved for merge (hitop-builder and hitop). Jeff chose F1, F2 and F5 as fix-now.

## Decisions

## Review

### Evidence (2026-09-21, builder c64ade9, hitop 151de796 R/)
- AC1: Fresh Playwright run on the page served from the branch (hitop 0.2.0 from r-universe). Box ticked on all four numbering and selection builds: `#shuffleNote` visible on each, and its first paragraph names `read_module()`, `score_hitopsr()` with `layout = "printed"`, and the default layout for columns in HiTOP-SR order. Read in full: no sentence says the columns must go back into instrument order before scoring.
- AC2: The page's own `bundleReadme()` extracted from builder main 1987f4a and the branch, version `0.2.0`, eight builds: six unshuffled READMEs byte-identical. The two shuffled Word READMEs remove 0 lines and add the same 4-line paragraph naming `score_hitopsr()`, `read_module()` and `layout = "printed"`. The three served shuffled bundles (module 1 to n, whole, module original numbers) each carry it.
- AC3: README.md §The scoring file names `score_hitopsr()` and `layout = "printed"` (and the shuffled Word README paragraph). §Shuffling the Word form names it in its scoring paragraph and the itemOrder sentence. The help-page sentence now reads that `generate_docx_hitopsr()` "gives the same scoring rule under its `randomize` argument", matching hitop `R/generate_docx.R:121-131` read this session.
- AC4: prose.mjs lists 154 passages, 17 mentioning a shuffled form or printed order, each read. The four that say how to score columns by order (body `#shuffleNote` p, readme:docx-shuffled, md:The scoring file, md:Shuffling the Word form) name `layout = "printed"` and the default layout for HiTOP-SR-order columns. The other 13 describe the controls, the scoring file, crosswalks or file names and give no scoring-by-order instruction. The unshuffled READMEs say only that `read_module()` reads the file back.
- AC5: Fresh bundles from the served branch page. Printed order read from each .docx item table by item text against `hitopsr_items$Text`, never from the .json. Seed 101, 50 respondents, items as positions: module 1 to n (13 items, 2 scales), whole instrument (405, 76 scales) and module with original numbers (13, 2): `layout = "printed"` on printed columns all.equal to the default layout on instrument-order columns, same module, in all three. Control: the default layout on printed columns differs in all three.
- AC6: `ste_lint.py --type descriptive` over all prose.mjs passages (page, four bundle READMEs, README.md): 0 findings. Smoke test 1/1 locally (9.2s). Builder PR https://github.com/jmgirard/hitop-builder/pull/17: CI `smoke` pass (57s, run 35638801679), at head including the fix-now commit. Re-lint after the fixes: 0.

### Consistency gate (2026-09-21)
- cairn_validate: all checks passed. Coverage complete. No principle changed, so cairn_impact skipped.
- hitop: `document()` no diff. `pkgdown::check_pkgdown()` no problems. README.Rmd untouched. NEWS: no hitop change, and hitop-builder keeps no NEWS. `devtools::check()` 0 errors, 0 warnings, 0 notes (4m 10s).

### Independent review (three lenses, fresh context)
- Diff-bug [O]: no correctness bug in the advice or the plumbing. Findings ranked:
  - F1 (medium): README.md:109-111 still says the warning reports "that a shuffled form is not scored as it stands", which contradicts the new notice and README.md:167.
  - F2 (medium): README.md:167-170 says the page's `#descriptorNote` "says the same" and every bundle README "says it again" right after the new `score_hitopsr()`/`layout` sentence. Only the shuffled Word README carries that advice.
  - F3 (process): AC6 needs the builder PR's CI, not yet run.
  - F4 (low): the advice does not tip `items` as positions to avoid `warn_item_order()` under `"printed"`.
  - F5 (low): on original numbering, the notice's "Data entered straight off the form has its columns in the order the form printed them" pulls against crosswalkSentence[0], "Enter each response under the number printed beside it".
  - F6 (low): README.md:378-379 "cannot be put back into instrument order" frames recovery as reordering (accurate).
  - F7 (low): unshuffled READMEs mention "on a shuffled Word form" (pre-existing, no AC4 conflict).
- Blame-history [S]: no finding. The change is M091's follow-through, and M092's prose rules and the writer ledger are intact.
- Prior-review [S]: no reintroduced or contradicted finding. GitHub probe empty. It flagged `--compare` on the new passage for a look. Checked: it pairs, and the added paragraph carries no facts, so it reports no difference (by design).

### Triage (step-7 gate, 2026-09-21)
- F1: fix now. Builder ebefd10: README.md says that the warning "says how to score a shuffled form".
- F2: fix now. Builder ebefd10: the `score_hitopsr()` sentence moved after "says it again", so "says the same" and "says it again" refer to `read_module()` only.
- F5: fix now. Builder ebefd10: the notice opens with "If you enter the data in the order the form printed the items, the columns are in printed order". The notice still names both layouts (AC1 holds).
- F3: process. AC6 is ticked only after builder CI is green.
- F4: rejected. The `score_hitopsr()` help already says that positions avoid the name warning, and the notice already runs to four sentences.
- F6: rejected. The sentence is accurate: without the `.json` there is no `item_order` to score through.
- F7: rejected. Pre-existing text, outside AC4's scoring clause.
- After the fixes: ste_lint 0 over all passages, smoke 1/1 (12.7s).
