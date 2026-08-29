# M062: Both files a builder download produces reach the visitor's disk

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m062-builder-download-handover` (hitop) → https://github.com/jmgirard/hitop/pull/69; builder PR https://github.com/jmgirard/hitop-builder/pull/7 on `m062-descriptor-handover` (rebased onto `8b30f96`)

## Goal

Hand the module descriptor over as a button the visitor clicks, so no browser can
withhold it as the second of two synthetic clicks and leave the questionnaire
saved with nothing to score it by.

## Scope

Surface tier: **user-facing** — the deployed page's visitors are external
consumers of this deliverable.

**In:** `download()`, `saveFile()` and the copy nodes around them in
`jmgirard/hitop-builder`'s `index.html`, plus that repo's `README.md`. The
questionnaire file keeps saving itself when the build completes; the descriptor
is held and offered as an enabled control naming its filename, replaced by the
next completed build.

**Out:** filenames that tell one build from another → M063. The version guard and
the install timeout → M064. The package's own console notice that it wrote a
descriptor → M065. Bundling both files into one archive → weighed and declined at
this plan gate (work log). The descriptor's JSON format is untouched.

## Acceptance criteria

- [x] AC1 For each of the six builds the driven run constructs — the page's own
      three `FORMATS` keys crossed with a selection of every scale (so
      `wholeInstrument()` is true) and a proper subset — the completed build
      calls `HTMLAnchorElement.prototype.click` exactly once, for the
      questionnaire file, and the descriptor's `click` is reached only with the
      provenance flag the descriptor control's own handler sets. Verified from
      the wrapper's recorded table of `download` attribute and flag per call.
- [x] AC2 The descriptor offer survives until taken or replaced: after a build
      the control is visible, enabled and names the descriptor's filename; it is
      still so after a move to the format step and back; a later completed build
      replaces the held descriptor and the page's log records the replacement.
      Probed over three orderings — Word (shuffled, one scale set) then Qualtrics
      (a different scale set), the same two in reverse, and a same-format
      rebuild — with the saved JSON asserted against the `scales` and `itemOrder`
      of the build that should be held.
- [x] AC3 Neither the R code the page evaluates nor the arguments it passes
      changes: for each of the three formats the call string the page logs equals
      the literal recorded from the merge base in T1, character for character.
- [x] AC4 Every copy node describing what a download produces — `#descriptorNote`,
      `#downloadHint`, the shuffle notice, and `README.md`'s *What the page shows*
      section — describes the two-step handover, each read and checked by id.
- [x] AC5 A real download is observed end to end: the maintainer builds one form
      in Chrome and one in Safari, and the milestone records for each browser
      which two files arrived in the downloads folder. Automation cannot see the
      downloads folder, so this criterion, not AC1, is what proves arrival.
- [ ] AC6 The change ships: a merged pull request in `jmgirard/hitop-builder`
      whose URL is in this file's header, and the page served at
      `https://jmgirard.github.io/hitop-builder/` matches that commit's
      `index.html` byte for byte.
- [x] AC7 The `hitop` package is untouched — `git diff --name-only` against the
      merge base lists only paths under `cairn/` — and `devtools::test()` is
      clean.

## Coverage

- AC1 → T2, T4
- AC2 → T2, T4
- AC3 → T1, T5
- AC4 → T3
- AC5 → T7
- AC6 → T6
- AC7 → T2, T3, T6

## Tasks

- [x] T1 Record the merge-base baseline: drive the deployed page once per format
      and copy the logged R call string and `extra`/`naming` values verbatim into
      this file, as the literals AC3 compares against.
- [x] T2 `saveFile()` takes a provenance flag; `download()` stops saving the
      descriptor and instead holds its bytes and filename and enables a
      `#saveDescriptor` control naming the file, whose handler sets the flag; a
      later completed build overwrites the held pair and logs the replacement
      (`hitop-builder/index.html:912-1024`).
- [x] T3 Rewrite `#descriptorNote`, `#downloadHint` and the shuffle notice
      (`index.html:499-512`, `:860-872`) and `README.md`'s *What the page shows*
      section for the two-step handover.
- [x] T4 Drive the six builds and the three replacement orderings in the browser
      pane, through `read_page` refs and `form_input` rather than screenshot
      coordinates, with the `click` wrapper and the M050 blob capture in place;
      record the tables AC1 and AC2 read.
- [x] T5 Compare each format's logged call string against T1's literal.
- [ ] T6 Open the builder pull request; after merge, fetch the deployed page and
      compare bytes; write the URL into the header.
- [x] T7 Hand the maintainer the Chrome and Safari runs and record what arrived.

## Evidence: baseline, driven run and real downloads (T1, T4, T5, T7)

Merge base = builder `main` at `8b30f96`. T1's baseline was driven on the page
deployed at `d046d03`, byte-identical to that commit, and no diff from there to
`8b30f96` or to the branch touches a call-construction line, so T1's literals
are the merge base's and the branch's alike. Selections: **A** =
`appearanceFocus` + `appetiteLoss` (2 of 76 scales, 8 items); **B** = all 76
(405 items, `wholeInstrument()` true). Both runs wrap
`HTMLAnchorElement.prototype.click` to record `download` and `data-origin`
without calling through, so no file reached a downloads folder.

The six logged call strings, identical base and branch character for character,
are `> generate_{docx,qualtrics,redcap}_hitopsr(file = out_path, descriptor =
desc_path`, then `, module = <2 scales>` under A and nothing under B, then `)`
· `, block_name = "HiTOP-SR", id_prefix = "HSR")` · `, form_name =
"hitopsr_questionnaire", required = TRUE)`. A planted `desc_path` → `desc2`
edit made the comparison report a difference, so it fails when it should.

Each baseline build fired two synthetic clicks; each branch build fires one,
`data-origin="build"`, for the questionnaire, and the descriptor's own button
then carried `data-origin="visitor"`, the value only that handler sets. The
control stays visible, enabled and named for the file after a build, across a
trip to the format step and back, and after *Clear all*, since it sits outside
`.downloads`, whose buttons `refreshTally()` disables. Three orderings —
Word-shuffled A → Qualtrics, the reverse, and a same-format rebuild taken after
each — were asserted against the held JSON's `scales` and `itemOrder`, and the
replacement log read correctly in each. `#descriptorNote`, `#downloadHint` and
`#shuffleCrosswalk`, read by id, describe the two-step handover in every
branch.

The 2026-08-29 layout move (builder `5a7cea0`) puts both buttons in one
`.downloadrow` flex row, `.downloads` and `.handover` staying separate
containers because `refreshTally()` disables `.downloads button`. Re-driven at
1100x900 for one A/docx build: both buttons on one row (`top` 296 each,
descriptor `left` 422 against download `right` 414), one `build` click then one
`visitor` click, and the descriptor button still enabled and named after *Clear
all*. `#descriptorNote` names no position, so AC4 holds in the stacked 375px
layout too.

T7: the maintainer built one form per browser on 2026-08-29 against the branch
page served from a local checkout, taking the descriptor with its own button;
automation never saw the folder. Chrome (Qualtrics, 2 scales) received
`hitopsr-module.txt` and `hitopsr-module.json`; Safari (REDCap, 2 scales)
received `hitopsr-module.zip` and `hitopsr-module-2.json`. Each pair is matched
by content, not timestamp: the `.txt`'s ten `[[ID:HSR_nnn]]` values are exactly
the first descriptor's `items`, the `.zip`'s six `instrument.csv` field names
exactly the second's. Both browsers delivered both files, so neither withheld
the descriptor once it was the visitor's own click. Safari renamed the second
descriptor because the first still sat in the folder, the case M063 fixes.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier); returned 6 findings on this milestone — a `userActivation` race, a non-existent whole-instrument toggle, an unobservable disk-arrival promise, an inert replacement probe, a one-exemplar replacement family, and a `git diff` with no sub-function scope — all fixed before the criteria were written.
- 2026-08-28: T1 — recorded the merge-base baseline from the deployed page (six builds, two selections x three formats); every build fired two synthetic anchor clicks.
- 2026-08-28: implement gate kept the plan's button-and-flag handover (a save started inside the visitor's own click carries fresh user activation, so it is not withheld), keeps the descriptor button offered after it is taken until the next build replaces it, and merges this milestone's builder pull request ahead of the parked scale-definitions one (PR #6, waiting on an r-universe rebuild).
- 2026-08-28: plan gate chose a held descriptor with its own button over one archive holding both files (rejected: only one of the three formats is an archive, so REDCap would nest, and the visitor must unzip) and over two buttons with nothing automatic (rejected: adds a click to every build for a hazard that only affects the second file); falsified by a report of the questionnaire file itself being withheld, which would mean no automatic save is safe.
- 2026-08-28: T2/T3 — `saveFile()` takes a `data-origin` argument; `download()` saves only the questionnaire and calls `offerDescriptor()`, which enables `#saveDescriptor` named for the file and logs any replacement; `takeDescriptor()` saves it with origin `visitor`. The control sits outside `.downloads` so `refreshTally()` cannot disable it. `#descriptorNote`, `#downloadHint`, both `crosswalkSentence()` branches naming the file, and README's *What the page shows*, *The scoring file* and *Shuffling the Word form* sections rewritten for the two-step handover. Builder commit `863e40c` on `m062-descriptor-handover`.
- 2026-08-28: T4/T5 — drove six builds and three replacement orderings on the branch; one `build` click per build, `visitor` on every descriptor click, all six logged call strings identical to T1's, and `git diff -U0 origin/main` touches no call-construction line.
- 2026-08-28: two defects in this session's own work, caught by the driven run and fixed before the evidence was recorded — the replacement notice claimed a loss even after the visitor had taken the file, and `#descriptorNote` said the second button appears beside the first when it appears under it.

- 2026-08-28: T6 first half — builder PR #7 opened from `m062-descriptor-handover`. Its second half (fetch the deployed page after merge and compare bytes) can only run past the merge, so it and AC6 land at the review gate; T6 stays unticked until then.
- 2026-08-28: AC7 verified — `git diff --name-only origin/main...HEAD` lists only `cairn/ROADMAP.md` and this file, and `devtools::test()` is FAIL 0 / WARN 0 / SKIP 4 / PASS 15504.
- 2026-08-29: at Jeff's direction the parked builder PR #6 (M057's scale-definition popups) was merged first, its r-universe blocker having cleared — the served `hitop` 0.2.0 is now built from `d6de830`, which carries the `Brief` column the popups read. `m062-descriptor-handover` was rebased onto the new `main` (`8b30f96`); the rebase applied cleanly, the branch diff is unchanged at 119/37 lines, and both features coexist. All six builds, the three replacement orderings and the copy checks were re-driven on the rebased branch; the evidence section above records the re-run, and PR #6 changing no call-construction line is what carries T1's literals to the new merge base.
- 2026-08-29: minor amendment at Jeff's request — the descriptor button moved from under the download button to beside it (`.downloadrow`), and `#descriptorNote` dropped its position claim so the copy is true in both the wide and the stacked layout. No criterion text changes; AC4's nodes re-read by id. Re-driven for one build rather than six, since the handover logic is untouched. Builder commit `5a7cea0`.
- 2026-08-29: the step-bar chevrons Jeff also asked for were routed out of this milestone at a question gate — `#stepbar` is nothing M062 touches — and went to their own builder PR https://github.com/jmgirard/hitop-builder/pull/8 off `main`, keeping PR #7 single-subject.

- 2026-08-29: T7 — the maintainer built one form in Chrome and one in Safari against the branch page and both pairs reached the downloads folder; each questionnaire's item set matches its own descriptor's `items` exactly. AC5 ticked. Safari's collision rename of the second descriptor is recorded as evidence for M063, not a defect here.

- 2026-08-29: implement closes with `devtools::test()` FAIL 0 / WARN 0 / SKIP 4 / PASS 15504 and `git diff --name-only origin/main...HEAD` still listing only the two `cairn/` paths. Every task but T6 is checked; T6's remaining half — fetch the deployed page and compare bytes — and AC6 with it cannot run before PR #7 merges, so they carry to the review gate as recorded on 2026-08-28.

- 2026-08-29: review gate failed before any acceptance criterion was verified — `cairn_validate.py` FAILs `weight caps`: this file's plan-owned body is 190 lines against the <150 cap (shed >=41), heaviest section `Evidence: baseline and driven run (T1, T4, T5)` at 68 lines. Status back to in-progress for a compressing rewrite; the `hitop` package itself is unaffected (`devtools::test()` FAIL 0 / WARN 0 / SKIP 4 / PASS 15504, and `git diff --name-only origin/main...HEAD` still lists only the two `cairn/` paths). Review PR https://github.com/jmgirard/hitop/pull/69 opened as a draft and left open.

- 2026-08-29: the two evidence sections were compressed into one and the milestone file is back under the plan-owned cap — 148 lines against <150, `cairn_validate` `weight caps` PASS; the driven-run tables and the downloads table became prose, keeping the six call-string literals AC3 compares against, the provenance flags, the three replacement orderings, the layout measurements and the per-browser file lists. `devtools::test()` FAIL 0 / WARN 0 / SKIP 4 / PASS 15504 and the branch diff still lists only the two `cairn/` paths. Status back to review.

- 2026-08-29: review pass 1 — AC1-AC4 and AC7 verified fresh (six driven builds, 12 clicks, six call strings identical to T1's literals with the comparison proven able to fail, copy read by id, `check()` 0/0/0, `test()` PASS 15504); AC5 verified against the recorded maintainer run on the same builder commit; AC6 open until builder PR #7 merges. Three-lens fan-out returned 9 findings, all from the [O] lens; the session reproduced its first on the live page.

- 2026-08-29: review gate — Jeff triaged the nine findings fix-now; builder `cc48256` closes the mid-build mismatch window, announces the handover in a polite live region, hides the empty container and un-indents the descriptor's log line, and the record corrections for findings 5 and 6 are in the Review section. AC1-AC4 re-verified against `cc48256`.

- 2026-08-29: AC5 re-run on builder `cc48256` at Jeff's selection — Chrome then Safari, both pairs arrived and each questionnaire's item set matches its own descriptor exactly. Merge authorized for builder PR #7 first, then hitop PR #69 once the deployed bytes match and CI is green.

## Decisions

- 2026-08-28: the descriptor button stays offered after the visitor takes it, until the next completed build replaces it. Saving twice costs nothing, and a first save that landed somewhere unintended would otherwise need a whole rebuild to recover from. The held pair carries a `taken` flag, so a replacement reports a loss only when the file was never taken.

## Review

Reviewed 2026-08-29 on `m062-builder-download-handover` (hitop `90524c7`) against
builder `m062-descriptor-handover` at `5a7cea0`, both containing their repo's
`origin/main` tip. The page was re-driven fresh from the local checkout served at
`http://localhost:8788/`, with `HTMLAnchorElement.prototype.click` wrapped to
record `download` and `data-origin` without calling through, so no file reached a
downloads folder.

### Acceptance criteria

- **AC1 — verified.** Six builds driven fresh: selection **A** = `appearanceFocus`
  + `appetiteLoss` (2 of 76, 8 items) and **B** = all 76 (405 items, `module`
  dropped from the call), each crossed with the three `FORMATS` keys. The click
  table holds 12 entries, strictly alternating: one `build` click per completed
  build, for the questionnaire only — `hitopsr-module.{docx,txt,zip}` under A,
  `hitopsr.{docx,txt,zip}` under B — each followed by one `visitor` click for the
  `.json`. No `.json` click carried `build`, and no build fired a second click.
- **AC2 — verified.** After each build the control is visible, `disabled = false`
  and named `Save the scoring file (<file>.json)`; it is still so after stepping
  out to the format step and back (checked on the Qualtrics panel, heading and
  download-button label both changed, handover button unchanged), and still
  enabled after *Clear all*, which disables the download button. Three orderings,
  each asserted against the taken JSON: Word-shuffled A untaken → Qualtrics with
  `agoraphobia`+`callousness`+`checking` held the Qualtrics build (`scales`
  Agoraphobia/Callousness/Checking, no `itemOrder`); the reverse held the
  Word-shuffled A build (`scales` Appearance Focus/Appetite Loss, `itemOrder`
  `[202,350,144,389,16,335,79,201]`); both logged "replaced before you took it".
  The same-format rebuild taken after each logged "the one you already saved is
  unaffected" and held a second, different shuffle
  (`[202,201,350,144,16,389,79,335]`).
- **AC3 — verified.** All six logged call strings compared mechanically against
  T1's literals as recorded at commit `03b190e`: 6 of 6 identical, 0 differ. The
  comparison is discriminating — replanting `desc_path` → `desc2` in one observed
  string makes it report exactly that one difference. `git diff -U0
  origin/main...HEAD` on the builder touches no call-construction line: filtering
  the diff for `evalRVoid`, `generate_`, `extra`, `naming`, `moduleArg`,
  `modulePrelude`, `out_path`, `desc_path`, `bind(`, and every generator argument
  name matches one added comment line and no code.
- **AC4 — verified.** Read by id on the running page: `#descriptorNote` opens "A
  download here is two files, and takes two clicks" and describes the second
  button; `#downloadHint` names both buttons, when each turns on, and that the
  offer persists; `#shuffleCrosswalk` was read in all three branches — the
  module/renumber and whole-instrument branches both name the `.json` the second
  button hands over, and the original-numbering branch describes the printed
  numbering only, having no download-content claim to make. `README.md`'s *What
  the page shows* section (and *The scoring file* and *Shuffling the Word form*)
  describes the two-step handover; no "This page saves two files" string survives
  in either file.
- **AC5 — verified, re-run against the shipped commit.** At Jeff's selection the
  test was repeated on builder `cc48256`, Chrome first and then Safari, against
  the branch page served from the local checkout. Four files reached the
  downloads folder. Chrome (Qualtrics, Bodily Distress + Entitlement) received
  `hitopsr-module.txt` 2737 B and `hitopsr-module.json` 261 B; Safari (REDCap,
  Hoarding + Manic Energy + Muscle Building) received `hitopsr-module.zip`
  1107 B and `hitopsr-module-2.json` 304 B. Each pair is matched by content, not
  timestamp: the `.txt`'s twelve `[[ID:HSR_nnn]]` values are exactly the first
  descriptor's `items` (10, 30, 58, 63, 95, 115, 133, 189, 324, 336, 371, 379),
  and the `.zip`'s eighteen `instrument.csv` field names are exactly the
  second's (9, 48, 77, 78, 105, 121, 129, 134, 178, 244, 279, 288, 319, 326,
  369, 372, 381, 396). Both browsers delivered both files. Safari again renamed
  its descriptor because the earlier one still sat in the folder — the M063
  case, recorded here as evidence rather than as a defect. The 2026-08-29 run
  against `5a7cea0` recorded above stands as the earlier observation.
- **AC6 — not verified; open.** Builder PR #7 is open, not merged, so no deployed
  page exists to compare bytes against. This is the gate's open item.
- **AC7 — verified.** `git diff --name-only origin/main...HEAD` lists
  `cairn/ROADMAP.md` and this file only. `devtools::test()` FAIL 0 / WARN 0 /
  SKIP 4 / PASS 15504.

### Consistency gate

`cairn_validate.py` exit 0, all checks PASS (21 advisories, all pre-existing:
20 dangling id tokens, 1 references-staleness). Toolchain slot: `document()`
leaves no diff, `pkgdown::check_pkgdown()` reports no problems, `devtools::check()`
is 0 errors / 0 warnings / 0 notes, and no NEWS entry is owed because the package
carries no user-visible change (AC7). No principle changed, so no impact report.

### Findings

Three fresh-context lenses. The blame-history and prior-PR-comment lenses each
returned no finding: the first records that this change is the documented
follow-up to a hazard M056's own review flagged and deferred, and that
`#saveDescriptor` being outside `.downloads` is deliberate and commented; the
second found both GitHub inline-comment probes empty and no archived review
finding contradicted. The [O] diff lens returned nine, ranked, listed with their
disposition at the gate.

### Findings and disposition

Nine findings, all from the [O] diff lens. Jeff triaged them at the gate on
2026-08-29; the fixes below landed on builder `cc48256` and every criterion was
re-verified against it.

1. **Fixed.** The descriptor button stayed clickable during the next build,
   still holding the previous build's JSON under the stem the new questionnaire
   was about to take, so a click mid-build handed over a matched-looking pair
   describing different forms. Reproduced before the fix: with a 2-scale
   descriptor held and untaken, a 3-scale build in flight, the button saved
   `hitopsr-module.json` carrying Appearance Focus and Appetite Loss while the
   `hitopsr-module.docx` that landed was the 3-scale form. `download()` now
   disables `#saveDescriptor` for the duration of a build; `offerDescriptor()`
   re-enables it with the new file, and a failed build restores the previous
   offer in the `finally`. Re-probed after the fix: mid-build the button reports
   `disabled = true` and a click records no visitor save; the take that follows
   the build carries the new build's three scales.
2. **Fixed by the same change.** A failed build left the previous build's
   descriptor on offer with only a log line; the offer is now restored
   deliberately, by a `finally` that re-enables only what is still held.
3. **Fixed.** The untaken-replacement warning reached only `#log`, which carries
   `aria-live="off"`.
4. **Fixed.** Nothing announced the second button's appearance, a regression for
   a screen-reader visitor who used to get the file automatically. A new
   `#handoverLive` polite region beside the buttons now says which scoring file
   is on offer, when a replacement took an untaken one away, and when a click
   saved one. `#downloadHint` and README gained the during-build clause so the
   copy stays true.
5. **Corrected here rather than in Tasks** (implement owns that section): T2's
   `index.html:912-1024` and T3's `:499-512`, `:860-872` were left behind by the
   `5a7cea0` layout move. At `5a7cea0` the real anchors were `saveFile` 955,
   `offerDescriptor` 981, `takeDescriptor` 1000, `#descriptorNote` 519,
   `crosswalkSentence()` 888; `cc48256` moves them again.
6. **Corrected.** The 2026-08-29 work-log line gives the rebased branch diff as
   "119/37 lines". `git diff --numstat origin/main...2b7667b` is 121 added / 35
   removed. The line stands as history; this is its correction.
7. **Fixed.** `.handover` is `hidden` until a build fills it, so the empty
   container no longer takes a gap beside the download button or a full-width
   row of its own under the narrow breakpoint.
8. **Fixed.** `takeDescriptor()`'s log line drops the two-space indent that
   marks output of the R call above it, which it does not follow.
9. **Acknowledged.** AC6 and T6's second half are open until builder PR #7
   merges; no criterion was ticked on the PR URL alone.

### Re-verification after the fixes (builder `cc48256`)

Driven fresh on the reloaded page, page controls actuated through the page's own
handlers and every save intercepted by the same `click` wrapper. Six builds:
12 clicks, one `build` per build for the questionnaire and one `visitor` per
descriptor, and zero `.json` clicks with any origin but `visitor`. The six
logged call strings compare 0-differ against T1's literals, and the whole
builder diff still touches no call-construction line. The three orderings hold:
Word-shuffled A untaken → Qualtrics held the Qualtrics build's three scales with
no `itemOrder`; the reverse held A's two with an `itemOrder`; the same-format
rebuild taken after each logged "the one you already saved is unaffected" and
held a second, different shuffle. The offer survives a move to the format step
and back and survives *Clear all*, which disables the download button. The three
`#shuffleCrosswalk` branches, `#descriptorNote` and `#downloadHint` read
correctly. Both buttons still report one row (`top` equal, download `right` 414
against descriptor `left` 422), and the handover container is `hidden` at load.
