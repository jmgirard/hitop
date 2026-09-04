# M087: The builder's start-up probe comments state what the probe establishes

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — source comments in the builder page's `index.html`. The visitor-facing README prose and the start-up log line M074 shipped carry the same trailing-gap imprecision the comments did; both are left to a follow-up candidate row rather than corrected here, so this milestone's diff stays comment-only
- **Branch/PR:** `m087-builder-probe-comments` (hitop, tracking) — https://github.com/jmgirard/hitop/pull/95 · `m087-probe-comments` (jmgirard/hitop-builder, code) — builder PR https://github.com/jmgirard/hitop-builder/pull/13

## Goal

The comments around the builder page's start-up `tilesExactly` probe say what it
establishes — that every scale's items together are exactly 1..N — instead of
attributing to it an overlap check `hitop_module()` makes impossible.

## Scope

**In:** the three comment blocks in `jmgirard/hitop-builder`'s `index.html` that
carry the claim — at `:765` (above `tilesExactly`), `:779` (above
`wholeInstrument()`) and `:1481` (above the R call) — rewritten to state the
probe's promise as coverage of 1..N and to say why overlap needs no check. The
`as.integer()` in the probe stays, with a comment saying the retention is
deliberate.

**Out:** removing the `as.integer()` → declined at this plan gate (see the work
log); it returns as a candidate row only if the version guard ever gains a way
to distinguish package builds. Any change to page behavior, the README, or the
`every scale ticked covers items 1..N with no gaps` log line → none is wrong.
Making the probe actually detect overlap → refused, not deferred: overlap does
not change the item set the gate turns on. Adding a second instrument to the
page → the standing candidate row on generalizing modularization.

## Acceptance criteria

- [x] AC1: Every comment block adjacent to an occurrence of `tilesExactly` or
      `wholeInstrument` in `index.html` — the occurrences enumerated by
      `grep -n 'tilesExactly\|wholeInstrument' index.html`, each read in full —
      states the probe's promise as what its expression establishes: the union
      of every scale's items runs from 1 upward with no gaps, and its own
      length is the only item count the probe reads. No block attributes
      overlap detection to the probe, and no block claims the probe can tell
      that the instrument has no further items above that run.
- [x] AC2: A case-insensitive grep for `overlap` over the builder repo's
      `index.html` and `README.md` returns only lines that either state the
      probe's promise correctly or say nothing about the probe.
- [x] AC3: The comment retaining the `as.integer()` states the reason the cast
      stays — that dropping it turns a coverage check into a type check, which
      the page's `MIN_HITOP` guard cannot back because every r-universe build of
      the package reports the same version.
- [x] AC4: `git diff` over the builder branch touches `index.html` comment lines
      only — no statement, no README line, no test.

## Coverage

- AC1 → T1, T2, T5
- AC2 → T3
- AC3 → T2
- AC4 → T4

## Tasks

- [x] T1: Record the basis for the corrected wording in this file: that
      `hitop_module()` de-duplicates at `R/module.R:102`, and the worked case —
      `hitopbr_scales` holds 67 item slots over 45 distinct items, 22 duplicated,
      whose union is exactly 1..45, so a probe of the shipped form returns TRUE
      on an overlapping instrument.
- [x] T2: Rewrite the three comment blocks (`index.html:765`, `:779`, `:1481`),
      each stating what the probe establishes, why overlap is immaterial (a union
      of 1..N is the same item set however scales share items), and — at the R
      call — why the `as.integer()` stays. (The three blocks sit at `:764`,
      `:774` and `:1483` on `main` at `016b2c8`, one line above the positions
      the plan recorded.)
- [x] T3: Grep both builder files case-insensitively for `overlap`; read every
      hit and confirm each either states the probe correctly or is unrelated
      (`index.html:205` is a CSS popup comment).
- [x] T4: Check the diff is comment-only, run the Playwright suite as a routine
      pre-PR check, and open the builder PR; record the PR URL here.
- [x] T5: Rewrite the three blocks again so each says the probe establishes a
      gap-free run from 1 and reads no separate item count, replacing the
      sentences that claimed it catches uncovered items; re-run the
      diff-is-comment-only check and the Playwright suite.

## Work log

- 2026-09-03: created by /milestone-plan; promoted from the ROADMAP candidate row
  added 2026-08-31 and extended 2026-09-02 (lineage M063, M074 finding 5, M081).
- 2026-09-03: plan gate chose keeping the probe's `as.integer()` over deleting it
  because deleting it converts a coverage check into a type check, and every
  r-universe build of the package reports `0.2.0`, so the page's `MIN_HITOP`
  guard cannot tell a pre-M081 build (double item numbers) from a later one — a
  visitor on the older build would silently get a module-headed Word form;
  falsified by the page gaining a way to identify the installed build, or by the
  package guaranteeing integer `$items` across every version the page accepts.
- 2026-09-03: plan gate chose comments that state why overlap needs no check
  over comments that state the check alone, because the bare statement is what
  let the overlap claim be written in twice; falsified by the added reasoning
  itself going stale.
- 2026-09-03: scope corrected against the candidate row, which named two comment
  sites; a third at `index.html:779` carries the claim in its flatly wrong form
  ("on an instrument whose scales overlap or leave gaps"). All three are in.
- 2026-09-03: reduced criteria audit ran ([O], fresh context, internal tier). It
  returned one instrument finding on the draft AC2 (a mandated Review-section
  quotation, rewritten to bind the text instead) and three findings on a draft
  AC3 binding the page's live behavior — unbounded against a Playwright suite
  asserting nothing about the probe, disproportionate to the internal tier, and
  binding the harness rather than the deliverable. AC3 was cut at the gate; the
  Playwright run stays a task-level check. AC1 was clean on all three questions.

- 2026-09-03: the R suite is red on `main` for a reason independent of this
  milestone — `test-item-number-merge-base.R:232`, whose guard against a
  merge base already carrying M086's retype now fires because the merge base
  does carry it. This branch's diff against `main` is two markdown files, so
  the failure predates it; captured as a ROADMAP candidate row rather than
  fixed here, which AC4 and the milestone's scope both bar.
- 2026-09-03: T4 — `git diff -U0 index.html` on the builder branch changes no
  non-comment line; `npm run smoke` passed locally against the branch checkout
  (15.8s, one test); builder PR https://github.com/jmgirard/hitop-builder/pull/13
  opened at `900feae`, its `smoke` check green (47s).
- 2026-09-03: T3 — `grep -in overlap index.html README.md` on the branch returns
  two hits and no README hit: `index.html:205`, the CSS comment on the
  definition popup overlapping the next row, unrelated to the probe; and
  `index.html:1487`, the rewritten block saying overlap is deliberately not
  asked about. The AC1 enumeration was swept too — the other two
  `wholeInstrument()` occurrences (`:1029`, `:1213`) carry no adjacent comment,
  code intervening at `:1213`.
- 2026-09-03: T2 — the three comment blocks rewritten in
  `jmgirard/hitop-builder` at `900feae` on `m087-probe-comments`; the diff
  changes comment lines only, and the `as.integer()` note was corrected before
  commit from "since hitop 0.2.0" to a version-free statement, since the older
  double-item builds report `0.2.0` as well.
- 2026-09-03: T1 — basis for the corrected wording recorded below as M087-D1;
  the de-duplication site and the HiTOP-BR worked case were both read from the
  package at `8592e803`.

- 2026-09-03: review step 3 — AC1-AC4 each executed against fresh evidence and
  ticked; evidence in the Review section. Consistency gate: `cairn_validate`
  exit 0 (16 PASS, advisories only), `document()` no diff,
  `pkgdown::check_pkgdown()` clean, no principle changed so no impact report.
  `devtools::test()` and the three review lenses still running at this
  checkpoint; hitop tracking PR https://github.com/jmgirard/hitop/pull/95
  opened draft.

- 2026-09-03: review returned M087 to `in-progress` (defect return 1). What
  failed: AC1. The probe's expression compares the scales' item union against
  its own length, so an instrument whose scales leave a trailing tail of items
  uncovered still makes it TRUE; the rewritten blocks at `index.html:765`,
  `:781-783` and `:1492` claim it catches uncovered items, and `:781-783`'s
  wording is this diff's own. Findings 3, 5 and 6 (the `tilesExactly`
  identifier, `:1490`'s "this gate", the README's matching imprecision) carry
  into the return; finding 4 was fixed in the Review text; finding 7 rejected
  as style. Consistency gate passed; `devtools::test()` red only on the
  pre-existing `test-item-number-merge-base.R:232`.

- 2026-09-03: amendment gate on the return — AC1's wording amended (substantive),
  the Surface tier line corrected, T5 added and Coverage's AC1 line extended to
  it. AC1 now states the probe's promise as a gap-free run from 1 whose own
  length is the only item count it reads, in place of "exactly 1..N", which the
  review read as coverage of the instrument. The amendment narrows AC1; no
  criterion was added, and none had its domain widened.
- 2026-09-03: re-audit: AC1 (reduced) — nothing.
- 2026-09-03: the same gate held the criteria set against the return's two
  carried findings. Finding 3 (the `tilesExactly` identifier asserts a partition
  the probe does not check) and finding 6 (the README at `:180` and `:283`, and
  the start-up log line, repeat the trailing-gap imprecision) each go to a
  follow-up candidate row rather than a scope widening, so the branch stays
  comment-only. Both are the residue of this milestone's own held candidate row:
  the post-merge hygiene pass graduates that row to them rather than retiring it
  outright.
- 2026-09-03: T5 — the three blocks rewritten in `jmgirard/hitop-builder`; each
  now states the promise as a gap-free run from 1, and the first and third say in
  so many words what the probe cannot see. `git diff -U0 index.html` leaves no
  changed line that is not a comment line, and `npm run smoke` passed (17.5s, one
  test). Finding 5 fixed in the same pass — the third block now says the union is
  all the *probe* looks at, where it had said the gate. Finding 2 answered by
  M087-D2 below, which supersedes M087-D1 rather than editing it.
- 2026-09-03: verify slot re-run after T5 — `devtools::test()` is
  `[ FAIL 1 | WARN 0 | SKIP 12 | PASS 17281 ]`, the one failure again
  `test-item-number-merge-base.R:232` with the same `expect_setequal(moved, ...)`
  message ("Absent: hitopsr_instructions, hitopbr_instructions"), the
  pre-existing red on `main` already carried as a candidate row; this branch's
  hitop diff is markdown only. Builder PR #13 green at `c472fbf` (smoke, 2m58s).
  Status set to `review`.

- 2026-09-03: review re-entry after the T5 repair (PR #95 open, AC1 unticked ->
  route (d)): default branch had not moved under either branch. AC1-AC4 each
  re-executed against `hitop-builder` `c472fbf` and ticked; evidence in the
  Review section's second pass. Consistency gate: `cairn_validate` exit 0 (16
  PASS, advisories only), `document()` no diff, `pkgdown::check_pkgdown()`
  clean, no principle changed. Blame-history and prior-review lenses returned no
  findings; the diff-bug lens and `devtools::test()` still running at this
  checkpoint.

- 2026-09-03: review lenses returned six findings, all from the diff-bug lens
  (blame-history and prior-review: none). Findings 1 and 2 fixed on the builder
  branch at `cf096d9` — the gate comment's closing clause asserted the coverage
  the block disclaims, and its FALSE-branch reasoning assumed the instrument's
  own numbering is 1..N. No return: neither falsifies a criterion inside its
  named procedure's domain nor is a defect in what the deliverables do. Findings
  3 and 6 join the graduation row; 4 and 5 rejected with reasons. `devtools::test()`
  `[ FAIL 1 | SKIP 12 | PASS 17281 ]`, the failure the pre-existing
  `test-item-number-merge-base.R:232` red, identity re-confirmed on its own.

- 2026-09-03: PR-conversation read before the gate — PR #95 and builder PR #13
  each returned no reviews, no conversation comments and no unresolved threads,
  so nothing was triaged and the blocking rule did not fire.
- 2026-09-03: step-7 approval: PR #95 approved for merge (builder PR #13
  approved in the same chip; that repo is not cairn-tracked, so its merge is
  gated by chat approval alone).

## Decisions

### M087-D1 (2026-09-03): The probe establishes coverage of 1..N, not the absence of overlap

`hitop_module()` builds its `items` as
`sort(unique(unlist(ref$itemNumbers[idx])))` (`R/module.R:102`), so the vector
the start-up probe reads is de-duplicated and ascending before
`identical(as.integer(.all$items), seq_along(.all$items))` is evaluated. The
probe therefore returns TRUE exactly when the union of the ticked scales' items
is 1..N with no gaps, whether or not those scales share items; overlap is
invisible to it. Worked case from this package's own keying:
`hitopbr_scales$itemNumbers` holds 67 item slots over 45 distinct items — 22
repeat slots across the 18 items that appear in more than one scale — and that
union is exactly 1..45, so the probe would return TRUE on an instrument whose
scales overlap heavily. Both figures measured 2026-09-03 by
`unlist(hitopbr_scales$itemNumbers)` against `hitop` at `8592e803`.

The corrected comments state this promise — the union is 1..N — and say why
overlap needs no check: a union of 1..N is the same item set however the scales
share items, and it is the item set, not the scale partition, that
`wholeInstrument()` turns on.

### M087-D2 (2026-09-03): The probe establishes a gap-free run from 1, not coverage of the instrument — supersedes M087-D1

M087-D1 said the probe "returns TRUE exactly when the union of the ticked
scales' items is 1..N with no gaps", and the corrected comments it authorized
said the same. Read as a statement about the instrument — which is how it reads
— that is wrong.
`identical(as.integer(.all$items), seq_along(.all$items))` compares the union
against its own length, so it is TRUE exactly when the union is 1..k for k the
number of distinct items the union holds. Nothing in the expression, and nothing
else at the call site, reads the instrument's own item count. Scales covering
1..k of a longer instrument therefore answer TRUE, `wholeInstrument()` goes
true, and the page drops `module` from a form that is not the whole instrument —
the mislabelling M087-D1 claimed the probe prevents. Measured 2026-09-03:
`identical(as.integer(1:20), seq_along(1:20))` is TRUE; `c(1:19, 30)`, a gap
below the maximum, is FALSE.

M087-D1's other half stands. `hitop_module()` de-duplicates at `R/module.R:102`,
so overlap is invisible to the probe and needs no check, and the HiTOP-BR worked
case there is unaffected. The corrected comments now state the promise as the
gap-free run and name both things the probe does not establish — items above the
run, and overlap. On the HiTOP-SR, the only instrument the page builds, the union
of all 76 scales is 405 items against `nrow(hitopsr_items)` 405, so the tail the
probe cannot see is not open there.

## Review

### First pass — returned on AC1

Evidence gathered 2026-09-03 against `jmgirard/hitop-builder` at `900feae`
(branch `m087-probe-comments`, PR #13) and `hitop` at `ca0c4fae`.

- AC1 — NOT met (see finding 1; the tick recorded earlier this pass was
  withdrawn). The enumeration below was executed as written, but two of the
  three blocks describe a promise the probe does not give.
  `grep -n 'tilesExactly\|wholeInstrument' index.html` returns eight
  occurrences (`:771`, `:781`, `:785`, `:786`, `:1029`, `:1213`, `:1501`,
  `:1508`); each was read with surrounding context. Three carry an adjacent
  comment block: `:765-770` (above `let tilesExactly`), `:777-784` (above
  `wholeInstrument()`, the `:781` occurrence being inside it) and `:1483-1501`
  (above the R call). Each states the promise as the union of the ticked
  scales' items being 1..N with no gaps, and none attributes overlap detection
  to the probe — `:767` says whether scales share items "is not asked and does
  not matter", `:783` says sharing items never puts the page in
  the module-keeping position, `:1487` says overlap is deliberately not asked
  about. The remaining
  five occurrences carry no adjacent comment: `:786` and `:1508` follow their
  own code, `:1029` opens a branch inside `crosswalkSentence()`, `:1213` is
  separated from the preceding paper-form comment by two statements, and `:771`
  and `:1501` are the declarations the first and third blocks head.
- AC2 — met. `grep -in overlap index.html README.md` returns two lines, both in
  `index.html` and no README hit. `:205` is a CSS comment on the definition
  popup overlapping the row below it, unrelated to the probe. `:1487` is the
  rewritten block saying overlap is deliberately not asked about and why
  (`hitop_module()` de-duplicates), which states the promise correctly.
- AC3 — met. The block at `:1495-1501` gives the reason the cast stays:
  without it the comparison is doubles against `seq_along()`'s integers and
  FALSE on any build made before `hitop_module()` returned integer items,
  "turning a coverage check into a type check", and `MIN_HITOP` cannot back a
  type assumption "because those older builds report 0.2.0 too -- the same
  version this page requires". No version number is claimed as the cutover.
- AC4 — met. `git diff main...HEAD --stat` on the builder branch: `index.html`
  only, 28 insertions and 15 deletions, no README and no test file. Filtering
  the `-U0` diff's changed lines to those not opening with `//`, `/*` or `*`
  leaves none, so every changed line is a comment line.

M087-D1's basis re-checked at `hitop` `ca0c4fae`: the de-duplicating line is
`R/module.R:102` (`items <- sort(unique(unlist(ref$itemNumbers[idx], ...)))`),
and `unlist(hitopbr_scales$itemNumbers)` gives 67 slots over 45 distinct items
with 18 items in more than one scale, its union exactly 1..45.

### Gate checks

`cairn_validate.py` exit 0 — all 16 PASS, three advisories (work-log line
wrapping, dangling pre-migration D-ids, one references page with no extraction
status), none a gate failure. No `DESIGN.md` principle changed, so no impact
report. Toolchain checks from the `r-package` profile's `consistency-gate`:
`devtools::document()` leaves no diff; `pkgdown::check_pkgdown()` reports no
problems; README.Rmd and NEWS.md are untouched and no user-visible package
behavior changed, so neither is owed an update; no new top-level file, so no
`.Rbuildignore` entry. `devtools::test()` is
`[ FAIL 1 | WARN 0 | SKIP 12 | PASS 17281 ]`; the single failure is
`test-item-number-merge-base.R:232` (`expect_setequal(moved, ...)` — Expected
"hitopsr_instructions", "hitopbr_instructions"; Absent both), the pre-existing
red already recorded on `main` at `8592e803` and carried as a ROADMAP candidate
row. This branch's diff against `main` is two markdown files, so it cannot have
caused it; CI is green on `main` because the block's `skip_without_merge_base()`
skips where no merge base is fetched.

### Findings

Three fresh-context reviewers, distinct evidence bases. The prior-review lens
and the blame-history lens each returned no findings; the diff-bug lens
returned seven, ranked, reproduced below with disposition.

1. **Fix required — the rewritten comments still overclaim.**
   `identical(as.integer(.all$items), seq_along(.all$items))` compares the
   union against *its own length*, so it establishes only that the union is a
   contiguous run starting at 1. The instrument's true item count is nowhere
   read. An instrument of N items whose scales cover only 1..k (k < N) makes
   the probe TRUE, `wholeInstrument()` true, and the page drop `module` — the
   mislabelling the comment claims the check prevents. Three new sentences are
   therefore false in that case: `:765` "cover its items exactly"; `:781-783`
   "on an instrument whose scales leave items uncovered, 'every scale' is a
   smaller item set than 'no module', and the page keeps passing the module";
   and `:1492` "the check is here so a future instrument leaving items
   uncovered trips it deliberately". Verified independently against the R
   expression and `wholeInstrument()`'s call sites. This is the error class the
   milestone exists to remove, and `:781-783`'s "leave items uncovered" is
   wording this diff introduced (`main` read "overlap or leave gaps"), so it is
   a defect inside an intentional change, not a pre-existing issue. It
   falsifies AC1 inside AC1's own enumeration, so it takes the return floor.
2. **Fix required — M087-D1 and the AC1 evidence repeat the imprecision.**
   "returns TRUE exactly when the union of the ticked scales' items is 1..N
   with no gaps" reads as coverage of the instrument. The decision record is
   append-only history, so it is superseded rather than edited when the
   comments are corrected.
3. **Carried into the return — the identifier `tilesExactly` still asserts a
   partition.** "Tile" means cover once without overlap, and it now sits above
   a comment saying overlap is not asked about. Renaming is a non-comment
   change that AC4 bars, so the return decides between a scope amendment and a
   candidate row.
4. **Fixed now — the AC1 evidence misparaphrased `:783`** as "the ungated
   position" where the comment says the module-keeping position. Corrected in
   the evidence text above.
5. **Carried into the return — `:1490` "the item set is all this gate turns
   on" is inexact.** `wholeInstrument()` also turns on the count of ticked
   scales; the item set is all the *probe* turns on.
6. **Carried into the return — the README repeats finding 1's imprecision**
   (`README.md:180` and `:283`, "cover its items with nothing left out"). The
   README is out of scope by declaration, but the Surface tier line's claim
   that its prose is "already accurate" does not hold in the trailing-gap
   sense, so that parenthetical is not a basis for leaving it alone.
7. **Rejected — two cosmetic nits** (an early line wrap at `:768`, a
   subject-agreement reading of "Scales sharing items"): pure style, the
   out-of-scope taxonomy's nitpick member.

**Disposition: defect return.** Finding 1 demonstrates AC1 failing inside the
domain of the procedure AC1 names, so the return floor applies: status goes
back to `in-progress` and review stops here. Defect returns for M087: 1.


### Second pass — after the T5 repair

Evidence gathered 2026-09-03 against `jmgirard/hitop-builder` at `c472fbf`
(branch `m087-probe-comments`, PR #13) and `hitop` at `3a64d8eb`. The default
branch had not moved under either branch, so this evidence is against the tree
that would merge.

- AC1 — met. `grep -n 'tilesExactly\|wholeInstrument' index.html` returns eight
  occurrences (`:774`, `:784`, `:792`, `:793`, `:1036`, `:1220`, `:1516`,
  `:1523`), each read in full with its surroundings. Three carry an adjacent
  comment block. `:765-773`, above `let tilesExactly`, states the promise as a
  run of item numbers starting at 1 with no gaps whose union is 1..k for k the
  number of distinct items it holds, says the probe "reads no separate count of
  the instrument's items, so it cannot see a tail of higher-numbered items that
  no scale claims", and says whether scales share items "is not asked and does
  not matter". `:780-791`, above `wholeInstrument()` (the `:784` occurrence
  falling inside it), states the gate as one-sided — "a gap-free run from 1 that
  stops short of the instrument's last item passes it, since nothing here reads
  that last item -- so it rules out one way of being wrong, not every way" — and
  says sharing items never puts the page in the module-keeping position.
  `:1490-1515`, above the R call, names both limits under its own heading ("Two
  things this does not establish"): items above the run, because `seq_along()`
  measures the union against itself, and overlap, because `hitop_module()`
  de-duplicates. No block attributes overlap detection to the probe, and none
  claims it can tell the instrument has no further items above the run. The
  other five occurrences carry no adjacent comment: `:792`/`:793` are the
  declaration the second block heads, `:1516` the call the third block heads,
  `:1036` a branch inside `crosswalkSentence()`, `:1523` a log line following
  its own code, and `:1220` is separated from the preceding paper-form comment
  by two intervening statements.
- AC2 — met. `grep -in overlap index.html README.md` returns two lines, both in
  `index.html`, no README hit. `:205` is a CSS comment about the definition
  popup overlapping the row below it, unrelated to the probe. `:1500` is the
  rewritten block saying overlap is deliberately not asked about and why, which
  states the promise correctly.
- AC3 — met. The block at `:1510-1515` gives the reason the cast stays: without
  it the line "would compare doubles to seq_along()'s integers and be FALSE on
  any build made before that change, turning a coverage check into a type
  check", and `MIN_HITOP` cannot back a type assumption "because those older
  builds report 0.2.0 too -- the same version this page requires". No version
  number is claimed as the cutover.
- AC4 — met. `git diff main...HEAD --stat` on the builder branch: `index.html`
  alone, 41 insertions and 13 deletions; no README, no test, no other file.
  Filtering the `-U0` diff's changed lines to those not opening with `//`, `/*`
  or `*` leaves none, so every changed line is a comment line.


**Re-verified after the fix-now edit** (`hitop-builder` `cf096d9`, findings 1
and 2). The block positions shift by the edit's two added lines: the three
comment blocks are now `:765-773`, `:779-793` and `:1492-1517`, heading the
`:774`, `:794` and `:1518` occurrences; the enumeration
`grep -n 'tilesExactly\|wholeInstrument' index.html` returns `:774`, `:784`,
`:794`, `:795`, `:1038`, `:1222`, `:1518`, `:1525`, the same eight, and the five
without an adjacent block are unchanged in kind. AC1 holds on the rewritten
block, which now says the gate "is not the coverage test it looks like" and
names both directions it can be wrong in. AC2: `grep -in overlap index.html
README.md` still returns `:205` (the CSS comment) and the probe block, now at
`:1502`, with no README hit. AC3's block is untouched by the edit. AC4:
`git diff main...HEAD --stat` is `index.html` alone, 43 insertions and 13
deletions, and filtering the `-U0` changed lines to non-comment lines leaves
none. `npm run smoke` passed at `cf096d9` (16.7s, one test).

#### Gate checks (second pass)

`cairn_validate.py` exit 0 — 16 PASS, three advisories (work-log wrapping,
dangling pre-migration D-ids, one references page with no extraction status),
none a gate failure; the `release window` advisory did not fire. No `DESIGN.md`
principle changed, so no impact report. Toolchain checks from the `r-package`
profile's `consistency-gate`: `devtools::document()` leaves the tree clean;
`pkgdown::check_pkgdown()` reports "No problems found"; README.Rmd, README.md
and NEWS.md are untouched and no user-visible package behavior changed, so
neither is owed an update; no new top-level file, so no `.Rbuildignore` entry.
The slot's full `R CMD check` ran on CI: all eight checks on PR #95 were green
at `3a64d8eb` — four `R CMD check` platforms (ubuntu release/devel/oldrel-1,
macOS, Windows), plus pkgdown, test-coverage and the line-endings guard.
Builder PR #13 was green at `c472fbf` (smoke), and `npm run smoke` passed
locally at `cf096d9` after the fix-now edit (16.7s, one test).

`devtools::test()` is `[ FAIL 1 | WARN 0 | SKIP 12 | PASS 17281 ]`. The single
failure is `test-item-number-merge-base.R:232`, re-run on its own to confirm the
identity: `expect_setequal(moved, ...)` — "Expected `moved` to have the same
values as c("hitopsr_instructions", "hitopbr_instructions") ... Absent: both".
This is the pre-existing red recorded on `main` at `8592e803` and carried as a
ROADMAP candidate row; this branch's `hitop` diff is markdown only, so it cannot
have caused it, and CI is green on `main` because the block's
`skip_without_merge_base()` skips where no merge base is fetched.

#### Findings (second pass)

Three fresh-context reviewers with distinct evidence bases. The blame-history
lens and the prior-review lens each returned no findings; the diff-bug lens
returned six, ranked, reproduced below with disposition.

1. **Fixed now — `index.html:788-790`, the closing sentence of the
   `wholeInstrument()` block, still asserts coverage.** "Scales sharing items
   never puts the page in the module-keeping position: the union of every scale
   is still the whole item set" — the justification clause states as fact
   exactly what the sentence three lines above disclaims ("a gap-free run from 1
   that stops short of the instrument's last item passes it"), so the block
   contradicts itself and reintroduces the coverage claim the milestone exists
   to remove. The point it is making is true and survives a fix; only the
   phrasing overreaches. Diff-introduced (`main` read "on an instrument whose
   scales overlap or leave gaps"), so a defect inside an intentional change.
   Fixed at `cf096d9`: the clause now reads "the union is the same item set
   however the scales share items". Not a return-floor finding — AC1's
   prohibitions are on attributing overlap detection to the probe and on
   claiming it can see items above the run, and this sentence is a claim about
   the union rather than about the probe; the criterion's positive requirement
   was met by the block before the fix as well.
2. **Fixed now — `index.html:784-786`'s FALSE-branch reasoning rests on an
   unstated assumption.** "Where the scales' items do not even run from 1
   without a gap, 'every scale' is plainly a smaller item set than 'no module'"
   holds only if the instrument's own item numbers are themselves 1..N; an
   instrument numbered non-contiguously would have its scales' union equal the
   whole instrument yet answer FALSE, making "plainly a smaller item set" false
   there. The behavior stays safe (it errs toward keeping the module), but the
   block flagged its one-sidedness only in the TRUE direction. Fixed at
   `cf096d9` in the same rewrite, which now names both directions the gate can
   be wrong in.
3. **Follow-up — the log line at `index.html:1523` still reads "every scale
   ticked covers items 1..N with no gaps"**, the same coverage imprecision the
   block above now spends a paragraph disclaiming, and `README.md:179-181` and
   `:283-285` repeat it ("cover its items with nothing left out"). Both are
   declared out of scope in the milestone and routed to a follow-up candidate
   row, so this is a scope observation rather than a defect. Same item as the
   first pass's finding 6; the graduation row the hygiene pass writes carries it.
4. **Rejected — `index.html:1509` calls the probe "a coverage check"**
   ("turning a coverage check into a type check") after two paragraphs
   establishing that it is not a coverage check, so the `as.integer()` rationale
   reuses the vocabulary the rest of the block retires. Rejected: AC3 binds this
   wording, so relabelling the phrase needs a criterion amendment, and the
   sentence's assertion — that dropping the cast makes the comparison turn on
   types rather than on item numbers — is true as written.
5. **Rejected — `index.html:765` "That is the whole of what the probe asks" is
   very slightly too absolute.** `as.integer()` truncates, so a hypothetical
   non-integral item vector such as `c(1.4, 2.4)` would pass, and an `NA` would
   fail on type rather than on coverage. Rejected as unreachable: `$items` comes
   from the shipped keying tables through `sort(unique(unlist(...)))`, so no such
   vector can arise, and the reviewer itself rated it worth a word only if the
   sentence were being edited anyway.
6. **Follow-up — `tilesExactly` still names a partition the probe does not
   check.** "Tile" means cover once without overlap, and the identifier sits
   directly above a comment saying overlap is not asked about and coverage is not
   established. Renaming is a non-comment change AC4 bars. Same item as the first
   pass's finding 3, already routed to a follow-up candidate row.

**Disposition: no return.** Findings 1 and 2 are comment inaccuracies fixed on
the branch; neither falsifies an acceptance criterion inside the domain of the
procedure that criterion names, and neither is a defect in what the package or
the page does for its users. Findings 3 and 6 go to the graduation row. Findings
4 and 5 are rejected with the reasons above. Defect returns for M087 stay at 1.
