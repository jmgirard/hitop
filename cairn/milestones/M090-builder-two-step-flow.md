# M090: A two-step builder flow with the format settings folded away

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M089
- **Driving RR:** —
- **Principles touched:** GP3 (page behavior under D-038; every setting keeps its gated default)
- **Resolves:** —
- **Surface tier:** user-facing — the public builder page's step flow and controls
- **Branch/PR:** `m090-builder-two-step-flow` (tracking, this repo); code on `m090-two-step-flow` in jmgirard/hitop-builder

## Goal

The HiTOP-SR Module Builder asks for scales, then for a format, and downloads from that second screen, with every format setting behind a closed disclosure that shows its current values.

## Scope

**In:** In `jmgirard/hitop-builder`, the three `section.step` blocks become two — *Choose scales* and *Choose a format and download* — and the step bar carries two buttons. A format card press (`[data-choose]`) marks the card `aria-current`, swaps in its `.fmtpanel` and relabels `#downloadBtn`, and no longer moves the step; the *Choose a different format* button goes. Each `.fmtpanel`'s settings — Word's paper size, numbering and shuffle; Qualtrics' block name and ID prefix; REDCap's form name and required flag — sit inside one `<details>` per format, closed on first paint and after every card press, whose `<summary>` names the current values and updates on change. The start-up log line at `index.html:1525`, the two `README.md` passages on ticking every scale (`README.md:179-181`, `283-285`), and the identifier `tilesExactly` are rewritten to say only what the probe establishes (absorbing the ROADMAP candidate row of 2026-09-03; lineage M063, M074, M081, M087). `README.md`'s *What the page shows* walks two steps; the smoke test drives the new flow. In this repo: tracking only.

**Out:** the one-file bundle → M089 (this milestone builds on it); a theme control and the client-facing scale text → their standing candidate rows; exposing further generator arguments → its standing candidate row (the disclosure is the home any promoted one would take).

## Acceptance criteria

- [ ] AC1: The booted page renders exactly two `section.step` elements and a step bar of two buttons — *Choose scales* and *Choose a format and download*; pressing any `[data-choose]` format card marks it `aria-current`, swaps in that format's `.fmtpanel`, relabels `#downloadBtn` for the format, and leaves the second step on screen — each read from the DOM after the press.
- [ ] AC2: Every format setting — enumerated by a DOM query as each named radio group plus each non-radio `input` inside `.fmtpanel` — sits inside a `<details>` element that is closed on first paint and after every format card press, whose `<summary>` names the current value of each of its settings; changing a setting's value updates that summary, read from the DOM after the change.
- [ ] AC3: With every disclosure left closed, the eight builds of M089's AC1 produce bundles whose questionnaire and `.json` entries match M089's captured bundles by M089's AC2 comparison; and with each setting moved off its default one at a time — A4, original numbering, shuffle, block `Wave 2 Screening`, prefix `W2SCR`, form `wave2_screening`, required unticked — the built questionnaire for that setting's format reflects it while the other two formats' questionnaires are unchanged by the same comparison.
- [ ] AC4: Every control that changes the visible step, enumerated by a DOM query for `[data-goto]` and `#stepbar button`, is crossed by a real Tab-then-activate sequence with the arrived-at step's heading focused and ringed; each `<summary>` is reached by Tab and toggled by Enter and by Space; and `#status` is the page's only announcing region, `#log` staying `aria-live="off"`, by a DOM query for `[aria-live]` and `[role=status]`.
- [ ] AC5: Across six cells — widths 360, 768 and 1280 px in light and dark — and each view (step one; step two per format with the disclosure closed and open), a containment check over every rendered control reports 0 clipped and no horizontal document scroll.
- [ ] AC6: The start-up log line, the two `README.md` passages on ticking every scale, and the identifier that carries the probe's answer each say only what the probe establishes — a gap-free run of scale items from 1 — with no claim that nothing is left out; the log line and both passages are quoted after the change, and a grep of the builder repo for `tilesExactly` and `nothing left out` returns no hit as a supporting check.
- [ ] AC7: `README.md`'s *What the page shows* walks two steps and names the disclosure; `tests/smoke.spec.js` drives the two-step flow to a bundle download; `node tests/plants.mjs` exits 0; the builder PR's smoke workflow is green.

## Coverage

- AC1 → T1, T6
- AC2 → T2, T6
- AC3 → T6
- AC4 → T6
- AC5 → T6
- AC6 → T3
- AC7 → T4, T5

## Tasks

- [x] T1: Merge steps two and three (`index.html:426-597`, `STEP_IDS`, `showStep()`, `setFormat()`, `markFormatChoice()`): cards select in place, the step bar drops to two buttons, the *Choose a different format* control and step three's heading swap go.
- [x] T2: Wrap each `.fmtpanel`'s fieldsets in a `<details>` with a `<summary>` rendered from the current values; close it in `setFormat()`; refresh the summary on `input`/`change`; token-layer styles for the summary and its focus ring.
- [x] T3: Rename `tilesExactly` and rewrite the `index.html:1525` log line and the two README passages to state a gap-free run from 1.
- [x] T4: Update `tests/smoke.spec.js` (`#stepbar button[data-goto="1"]` then `[data-choose="docx"]`, `smoke.spec.js:117-124`) for the two-step flow and keep `tests/plants.mjs` green.
- [x] T5: Rewrite `README.md`'s *What the page shows* for two steps and the disclosure.
- [ ] T6: Run the AC1–AC5 checks on the served branch: DOM reads, the eight-build comparison against M089's captures, the seven off-default probes, the keyboard walk, the six-cell containment sweep; record digests in the work log.

## Work log

- 2026-09-11: created by /milestone-plan.
- 2026-09-11: criteria audit ran in full mode ([O] fresh reader): M090 AC2 enumerates settings as radio groups plus inputs, AC3 fixes one probe value per text setting and points "unchanged" at M089's comparison, AC4 enumerates step-changing controls by DOM query and states `#log` as non-announcing, AC5 names its widths, AC6 quotes the passages with the grep demoted to a supporting check.
- 2026-09-11: plan gate chose two steps with cards selecting in place over keeping three steps with folded options, or one unstepped page, because it removes a screen while keeping M053's one-format-on-screen promise; falsified by visitors reporting the download as hard to find on the second step.
- 2026-09-11: plan gate chose folding all seven settings over keeping paper size and the required flag visible because every setting has a gated package default and the summary line shows the values; falsified by a visitor missing a non-default they needed (A4 the likeliest).
- 2026-09-11: plan gate chose absorbing the log-line candidate row here over leaving it because this milestone edits the README sections its promotion condition names.
- 2026-09-11: implement started; branches `m090-builder-two-step-flow` (here) and `m090-two-step-flow` (builder). Question gate: Jeff asked for light beautification alongside the redesign; taken as a minor amendment within T1/T2's styling — every new colour reads from the existing token layer, so the contrast promises stand, and AC5's six-cell sweep covers the new elements. No other choice was open.
- 2026-09-11: T1+T2 landed in one builder commit (the merged markup and the disclosures are one region): `STEP_IDS` two entries, `setFormat()` closes every `details`, marks the card and relabels the button, card press no longer calls `showStep()`; `FORMATS[].heading` and `#step3h` gone; `refreshSummaries()`/`settingsSummary()` write each `[data-summary]` span from the controls on `input`/`change`. Styling: step-number discs, current-card check, disclosure chevron, larger download button. Screenshots at 360/768/1280 in both schemes read clean by eye.
- 2026-09-11: T3–T5 in one builder commit: `tilesExactly` → `itemsRunFromOne`; log line now "the scales' items together run from 1 with no gaps: true"; the naming-section and *Ticking every scale* passages state the gap-free run and the tail the probe cannot see; the mid-build "keeping the module" log line dropped its "cover … 1..N" wording too. *What the page shows* walks two steps and names the disclosure and its opening summary. `smoke.spec.js` keeps the `data-goto="1"` then `[data-choose="docx"]` route (still the visitor's path) with its comment rewritten; two "On the Word screen" leads became "Under *Word settings*".

## Decisions

## Review
