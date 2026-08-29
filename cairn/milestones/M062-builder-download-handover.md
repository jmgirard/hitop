# M062: Both files a builder download produces reach the visitor's disk

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m062-builder-download-handover` (hitop); builder branch `m062-descriptor-handover` in `jmgirard/hitop-builder`

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

- [ ] AC1 For each of the six builds the driven run constructs — the page's own
      three `FORMATS` keys crossed with a selection of every scale (so
      `wholeInstrument()` is true) and a proper subset — the completed build
      calls `HTMLAnchorElement.prototype.click` exactly once, for the
      questionnaire file, and the descriptor's `click` is reached only with the
      provenance flag the descriptor control's own handler sets. Verified from
      the wrapper's recorded table of `download` attribute and flag per call.
- [ ] AC2 The descriptor offer survives until taken or replaced: after a build
      the control is visible, enabled and names the descriptor's filename; it is
      still so after a move to the format step and back; a later completed build
      replaces the held descriptor and the page's log records the replacement.
      Probed over three orderings — Word (shuffled, one scale set) then Qualtrics
      (a different scale set), the same two in reverse, and a same-format
      rebuild — with the saved JSON asserted against the `scales` and `itemOrder`
      of the build that should be held.
- [ ] AC3 Neither the R code the page evaluates nor the arguments it passes
      changes: for each of the three formats the call string the page logs equals
      the literal recorded from the merge base in T1, character for character.
- [ ] AC4 Every copy node describing what a download produces — `#descriptorNote`,
      `#downloadHint`, the shuffle notice, and `README.md`'s *What the page shows*
      section — describes the two-step handover, each read and checked by id.
- [ ] AC5 A real download is observed end to end: the maintainer builds one form
      in Chrome and one in Safari, and the milestone records for each browser
      which two files arrived in the downloads folder. Automation cannot see the
      downloads folder, so this criterion, not AC1, is what proves arrival.
- [ ] AC6 The change ships: a merged pull request in `jmgirard/hitop-builder`
      whose URL is in this file's header, and the page served at
      `https://jmgirard.github.io/hitop-builder/` matches that commit's
      `index.html` byte for byte.
- [ ] AC7 The `hitop` package is untouched — `git diff --name-only` against the
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
- [ ] T7 Hand the maintainer the Chrome and Safari runs and record what arrived.

## Evidence: baseline and driven run (T1, T4, T5)

Merge base = builder `main` at `d046d03`, which the deployed page matched byte
for byte on 2026-08-28 (sha256 `0386d1c4…73450`); the baseline was driven there,
the branch from a local server. Selections: **A** = `appearanceFocus` +
`appetiteLoss` (2 of 76 scales, 8 items); **B** = all 76 (405 items), so
`wholeInstrument()` is true. Format options at the page's defaults. Both runs
wrap `HTMLAnchorElement.prototype.click` to record `download` and the anchor's
`data-origin` without calling through, so the tables are what the page
attempted and no file reached a downloads folder; controls are driven through
`read_page` refs, never screenshot coordinates.

| Build | Logged call string (identical merge base and branch, character for character) | Base clicks | Branch clicks |
|---|---|---|---|
| A docx | `> generate_docx_hitopsr(file = out_path, descriptor = desc_path, module = <2 scales>)` | `.docx`, `.json` | `.docx` (build) |
| A qualtrics | `> generate_qualtrics_hitopsr(file = out_path, descriptor = desc_path, module = <2 scales>, block_name = "HiTOP-SR", id_prefix = "HSR")` | `.txt`, `.json` | `.txt` (build) |
| A redcap | `> generate_redcap_hitopsr(file = out_path, descriptor = desc_path, module = <2 scales>, form_name = "hitopsr_questionnaire", required = TRUE)` | `.zip`, `.json` | `.zip` (build) |
| B docx | `> generate_docx_hitopsr(file = out_path, descriptor = desc_path)` | `.docx`, `.json` | `.docx` (build) |
| B qualtrics | `> generate_qualtrics_hitopsr(file = out_path, descriptor = desc_path, block_name = "HiTOP-SR", id_prefix = "HSR")` | `.txt`, `.json` | `.txt` (build) |
| B redcap | `> generate_redcap_hitopsr(file = out_path, descriptor = desc_path, form_name = "hitopsr_questionnaire", required = TRUE)` | `.zip`, `.json` | `.zip` (build) |

Each of the six baseline builds fired two synthetic clicks; each branch build
fires one, `data-origin="build"`, for the questionnaire. In every one of the six
the descriptor's own button was then pressed and its click carried
`data-origin="visitor"`, the value only that button's handler sets. The
`extra`/`naming` strings the log does not print — docx `, papersize =
paper_size, renumber = TRUE, randomize = FALSE`; qualtrics `, block_name =
block_name, id_prefix = id_prefix`; redcap `, form_name = form_name, required =
TRUE` — are unchanged: `git diff -U0 origin/main` touches no line that builds
the R call, its arguments, or the logged string. A planted `desc_path` →
`desc2` edit made the six-string comparison report a difference, so it can fail.

After a build the control is visible, enabled and named for the file
(`Save the scoring file (hitopsr-module.json)`), still so after stepping out to
the format screen and back, and still enabled after *Clear all* empties the
selection and disables the download button — it sits outside `.downloads`, whose
buttons `refreshTally()` disables. Three replacement orderings:

| Ordering | Held after the second build | Log |
|---|---|---|
| Word shuffled A → Qualtrics with `agoraphobia`+`callousness`+`checking` | `scales` = those three, no `itemOrder` | "replaced before you took it" |
| the same two reversed | `scales` = A's two, `itemOrder` `[79,202,16,201,335,350,389,144]` | "replaced before you took it" |
| Word shuffled A rebuilt, taken after each | two `itemOrder`s, `[16,201,202,79,335,389,350,144]` then `[79,201,16,350,202,389,335,144]` | "the one you already saved is unaffected" |

Copy read by id from the running page: `#descriptorNote` opens "A download here
is two files, and takes two clicks"; `#downloadHint` names both buttons and when
each turns on; `#shuffleCrosswalk` reads correctly in all three of its branches,
the whole-instrument one ending "take it, or the order is lost."

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier); returned 6 findings on this milestone — a `userActivation` race, a non-existent whole-instrument toggle, an unobservable disk-arrival promise, an inert replacement probe, a one-exemplar replacement family, and a `git diff` with no sub-function scope — all fixed before the criteria were written.
- 2026-08-28: T1 — recorded the merge-base baseline from the deployed page (six builds, two selections x three formats); every build fired two synthetic anchor clicks.
- 2026-08-28: implement gate kept the plan's button-and-flag handover (a save started inside the visitor's own click carries fresh user activation, so it is not withheld), keeps the descriptor button offered after it is taken until the next build replaces it, and merges this milestone's builder pull request ahead of the parked scale-definitions one (PR #6, waiting on an r-universe rebuild).
- 2026-08-28: plan gate chose a held descriptor with its own button over one archive holding both files (rejected: only one of the three formats is an archive, so REDCap would nest, and the visitor must unzip) and over two buttons with nothing automatic (rejected: adds a click to every build for a hazard that only affects the second file); falsified by a report of the questionnaire file itself being withheld, which would mean no automatic save is safe.
- 2026-08-28: T2/T3 — `saveFile()` takes a `data-origin` argument; `download()` saves only the questionnaire and calls `offerDescriptor()`, which enables `#saveDescriptor` named for the file and logs any replacement; `takeDescriptor()` saves it with origin `visitor`. The control sits outside `.downloads` so `refreshTally()` cannot disable it. `#descriptorNote`, `#downloadHint`, both `crosswalkSentence()` branches naming the file, and README's *What the page shows*, *The scoring file* and *Shuffling the Word form* sections rewritten for the two-step handover. Builder commit `863e40c` on `m062-descriptor-handover`.
- 2026-08-28: T4/T5 — drove six builds and three replacement orderings on the branch; one `build` click per build, `visitor` on every descriptor click, all six logged call strings identical to T1's, and `git diff -U0 origin/main` touches no call-construction line.
- 2026-08-28: two defects in this session's own work, caught by the driven run and fixed before the evidence was recorded — the replacement notice claimed a loss even after the visitor had taken the file, and `#descriptorNote` said the second button appears beside the first when it appears under it.

## Decisions

- 2026-08-28: the descriptor button stays offered after the visitor takes it, until the next completed build replaces it. Saving twice costs nothing, and a first save that landed somewhere unintended would otherwise need a whole rebuild to recover from. The held pair carries a `taken` flag, so a replacement reports a loss only when the file was never taken.

## Review
