# M087: The builder's start-up probe comments state what the probe establishes

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — source comments in the builder page's `index.html`; the visitor-facing README prose and log line M074 shipped are already accurate and are not touched
- **Branch/PR:** `m087-builder-probe-comments` (hitop, tracking) · `m087-probe-comments` (jmgirard/hitop-builder, code)

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

- [ ] AC1: Every comment block adjacent to an occurrence of `tilesExactly` or
      `wholeInstrument` in `index.html` — the occurrences enumerated by
      `grep -n 'tilesExactly\|wholeInstrument' index.html`, each read in full —
      describes the probe's promise as the union of every scale's items being
      exactly 1..N, and attributes no overlap detection to it.
- [ ] AC2: A case-insensitive grep for `overlap` over the builder repo's
      `index.html` and `README.md` returns only lines that either state the
      probe's promise correctly or say nothing about the probe.
- [ ] AC3: The comment retaining the `as.integer()` states the reason the cast
      stays — that dropping it turns a coverage check into a type check, which
      the page's `MIN_HITOP` guard cannot back because every r-universe build of
      the package reports the same version.
- [ ] AC4: `git diff` over the builder branch touches `index.html` comment lines
      only — no statement, no README line, no test.

## Coverage

- AC1 → T1, T2
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
- [ ] T3: Grep both builder files case-insensitively for `overlap`; read every
      hit and confirm each either states the probe correctly or is unrelated
      (`index.html:205` is a CSS popup comment).
- [ ] T4: Check the diff is comment-only, run the Playwright suite as a routine
      pre-PR check, and open the builder PR; record the PR URL here.

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

- 2026-09-03: T2 — the three comment blocks rewritten in
  `jmgirard/hitop-builder` at `900feae` on `m087-probe-comments`; the diff
  changes comment lines only, and the `as.integer()` note was corrected before
  commit from "since hitop 0.2.0" to a version-free statement, since the older
  double-item builds report `0.2.0` as well.
- 2026-09-03: T1 — basis for the corrected wording recorded below as M087-D1;
  the de-duplication site and the HiTOP-BR worked case were both read from the
  package at `8592e803`.

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

## Review
