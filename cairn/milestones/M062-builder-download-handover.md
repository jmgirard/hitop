# M062: Both files a builder download produces reach the visitor's disk

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** —

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

- [ ] T1 Record the merge-base baseline: drive the deployed page once per format
      and copy the logged R call string and `extra`/`naming` values verbatim into
      this file, as the literals AC3 compares against.
- [ ] T2 `saveFile()` takes a provenance flag; `download()` stops saving the
      descriptor and instead holds its bytes and filename and enables a
      `#saveDescriptor` control naming the file, whose handler sets the flag; a
      later completed build overwrites the held pair and logs the replacement
      (`hitop-builder/index.html:912-1024`).
- [ ] T3 Rewrite `#descriptorNote`, `#downloadHint` and the shuffle notice
      (`index.html:499-512`, `:860-872`) and `README.md`'s *What the page shows*
      section for the two-step handover.
- [ ] T4 Drive the six builds and the three replacement orderings in the browser
      pane, through `read_page` refs and `form_input` rather than screenshot
      coordinates, with the `click` wrapper and the M050 blob capture in place;
      record the tables AC1 and AC2 read.
- [ ] T5 Compare each format's logged call string against T1's literal.
- [ ] T6 Open the builder pull request; after merge, fetch the deployed page and
      compare bytes; write the URL into the header.
- [ ] T7 Hand the maintainer the Chrome and Safari runs and record what arrived.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier); returned 6 findings on this milestone — a `userActivation` race, a non-existent whole-instrument toggle, an unobservable disk-arrival promise, an inert replacement probe, a one-exemplar replacement family, and a `git diff` with no sub-function scope — all fixed before the criteria were written.
- 2026-08-28: plan gate chose a held descriptor with its own button over one archive holding both files (rejected: only one of the three formats is an archive, so REDCap would nest, and the visitor must unzip) and over two buttons with nothing automatic (rejected: adds a click to every build for a hazard that only affects the second file); falsified by a report of the questionnaire file itself being withheld, which would mean no automatic save is safe.

## Decisions

## Review
