# M062: Both files a builder download produces reach the visitor's disk

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m062-builder-download-handover` (hitop); builder PR https://github.com/jmgirard/hitop-builder/pull/7 on `m062-descriptor-handover` (rebased onto `8b30f96`)

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
- [x] AC5 A real download is observed end to end: the maintainer builds one form
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
- [x] T7 Hand the maintainer the Chrome and Safari runs and record what arrived.

## Evidence: baseline and driven run (T1, T4, T5)

Merge base = builder `main` at `8b30f96` (`d046d03` plus the merged
scale-definition popups). T1's baseline was driven on the deployed page at
`d046d03` (sha256 `0386d1c4…73450`, matching byte for byte); `git diff -U0
d046d03 8b30f96` touches no call-construction line, so its literals are the
merge base's. The branch was re-driven locally after rebasing onto `8b30f96`.
Selections: **A** = `appearanceFocus` + `appetiteLoss` (2 of 76 scales, 8
items); **B** = all 76 (405 items), so `wholeInstrument()` is true; format
options at the page's defaults. Both runs wrap
`HTMLAnchorElement.prototype.click` to record `download` and `data-origin`
without calling through, so the tables are what the page attempted and no file
reached a downloads folder; controls are driven through `read_page` refs.

| Build | Logged call string (identical merge base and branch, character for character) | Base | Branch |
|---|---|---|---|
| A docx | `> generate_docx_hitopsr(file = out_path, descriptor = desc_path, module = <2 scales>)` | `.docx`, `.json` | `.docx` |
| A qualtrics | `> generate_qualtrics_hitopsr(file = out_path, descriptor = desc_path, module = <2 scales>, block_name = "HiTOP-SR", id_prefix = "HSR")` | `.txt`, `.json` | `.txt` |
| A redcap | `> generate_redcap_hitopsr(file = out_path, descriptor = desc_path, module = <2 scales>, form_name = "hitopsr_questionnaire", required = TRUE)` | `.zip`, `.json` | `.zip` |
| B docx | `> generate_docx_hitopsr(file = out_path, descriptor = desc_path)` | `.docx`, `.json` | `.docx` |
| B qualtrics | `> generate_qualtrics_hitopsr(file = out_path, descriptor = desc_path, block_name = "HiTOP-SR", id_prefix = "HSR")` | `.txt`, `.json` | `.txt` |
| B redcap | `> generate_redcap_hitopsr(file = out_path, descriptor = desc_path, form_name = "hitopsr_questionnaire", required = TRUE)` | `.zip`, `.json` | `.zip` |

Each baseline build fired two synthetic clicks; each branch build fires one,
`data-origin="build"`, for the questionnaire. In all six the descriptor's own
button was then pressed and its click carried `data-origin="visitor"`, the value
only that handler sets. The `extra`/`naming` strings the log omits — docx `,
papersize = paper_size, renumber = TRUE, randomize = FALSE`; qualtrics `,
block_name = block_name, id_prefix = id_prefix`; redcap `, form_name =
form_name, required = TRUE` — are unchanged: `git diff -U0 origin/main` touches
no call-construction line. A planted `desc_path` → `desc2` edit made the
six-string comparison report a difference, so it can fail.

After a build the control is visible, enabled and named for the file, still so
after stepping out to the format screen and back, and still enabled after
*Clear all* empties the selection and disables the download button — it sits
outside `.downloads`, whose buttons `refreshTally()` disables. Three orderings:

| Ordering | Held after the second build | Log |
|---|---|---|
| Word shuffled A → Qualtrics with `agoraphobia`+`callousness`+`checking` | those three, no `itemOrder` | "replaced before you took it" |
| the same two reversed | A's two, `itemOrder` `[350,335,79,202,16,144,389,201]` | "replaced before you took it" |
| Word shuffled A rebuilt, taken after each | `[335,79,202,201,350,389,144,16]` then `[202,335,201,389,79,350,144,16]` | "the one you already saved is unaffected" |

Copy read by id: `#descriptorNote` opens "A download here is two files, and
takes two clicks"; `#downloadHint` names both buttons and when each turns on;
`#shuffleCrosswalk` reads correctly in all three branches, the whole-instrument
one ending "take it, or the order is lost." The rebase leaves the picker's 76
definition popups intact and non-empty.

### Layout revision (2026-08-29)

At Jeff's request the descriptor button now sits *beside* the questionnaire
button rather than under it. `.downloads` and `.handover` share one
`.downloadrow` flex row; they stay separate containers, because
`refreshTally()` disables `.downloads button` and must not reach the handover
button. Re-driven on the branch at 1100x900 (one build, A/docx, not the whole
six): the two buttons report `top` 296 both, the descriptor's `left` 422 against
the download's `right` 414 — one row, an 8px `--s2` gap — and the clicks are one
`build` for `hitopsr-module.docx` then one `visitor` for `hitopsr-module.json`.
After *Clear all* the download button is `disabled` and the descriptor button is
still shown, enabled and named `Save the scoring file (hitopsr-module.json)`, so
the separation invariant survives the regrouping. At 375px the row's children go
full width and stack, both at `left` 16. `#descriptorNote` no longer names a
position — "A second button then appears" — since only the wide layout puts it
beside; re-read by id, it and `#downloadHint` still describe the two-step
handover, so AC4 holds. Builder commit `5a7cea0`.

## Evidence: the real downloads (T7, AC5)

Run by the maintainer on 2026-08-29 against the branch page served from the
local checkout at `http://localhost:8087/`, one build per browser, taking the
descriptor with its own button each time. Automation never saw the folder; the
four files below are what `ls -lT ~/Downloads` reported afterwards.

| Browser | Build | Files that arrived | Descriptor's scales |
|---|---|---|---|
| Chrome | Qualtrics, 2 scales | `hitopsr-module.txt` 2337 B (23:10:04), `hitopsr-module.json` 258 B (23:10:09) | Body Dissatisfaction, Callousness |
| Safari | REDCap, 2 scales | `hitopsr-module.zip` 738 B (23:11:11), `hitopsr-module-2.json` 248 B (23:11:13) | Binge Eating, Difficulties Reaching Orgasm |

Each pair is matched from the file contents, not from the timestamps: the
`.txt`'s ten `[[ID:HSR_nnn]]` values are exactly the first descriptor's `items`
(34, 43, 67, 211, 236, 250, 255, 271, 329, 395), and the `.zip`'s six
`instrument.csv` field names are exactly the second's (82, 124, 151, 358, 392,
398). Two files arrived per build in both browsers, so no browser withheld the
descriptor once it was the visitor's own click.

Safari renamed the second descriptor `hitopsr-module-2.json` because the first
still sat in the folder. Nothing was lost and nothing here is wrong, but a
visitor who builds twice ends up with two identically-stemmed scoring files and
nothing on either naming the build it belongs to — the case M063 exists to fix.

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

## Decisions

- 2026-08-28: the descriptor button stays offered after the visitor takes it, until the next completed build replaces it. Saving twice costs nothing, and a first save that landed somewhere unintended would otherwise need a whole rebuild to recover from. The held pair carries a `taken` flag, so a replacement reports a loss only when the file was never taken.

## Review
