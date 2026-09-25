# M126: The module builder's format step gains an "Online form" card that saves the `write_module()` scoring file and links to hitop-form's link builder with the module filled in

- **Status:** planned
- **Priority:** normal
- **Depends on:** M125
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — a researcher-facing control on the deployed builder page
- **Branch/PR:** —

## Goal

Offer the online form beside Word, Qualtrics and REDCap in the builder's format step, so choosing it saves the scoring descriptor and opens hitop-form's link builder with the module already in place.

## Scope

**In:** `index.html` in `jmgirard/hitop-builder`: a fourth format card, a `download()` branch that writes the descriptor with `hitop::write_module()` under webR and saves it as one `.json` file, an anchor to `link.html?c=` carrying `{ instrument, module }` in the form page's own encoding, and the notice and hint copy for that card. The smoke assertions, plants and prose ledger for it, an `npm run prose` script, and a `smoke.yml` step that runs the ledger. The README. The card is page behavior under D-038: `write_module()` and `hitop_module()` are signed-off package surfaces and no participant-facing text moves.

**Out:** `link.html` reading the parameter → M125 (this milestone waits on it). Widening the prose ledger's grep to the four escaping writer forms → the candidate row, narrowed. A shuffle setting for the online card → the link builder's own "random order" box covers it. The package site → M124.

## Acceptance criteria

- [ ] AC1: Step 2 shows four format cards and the fourth is labelled "Online form". With it chosen, pressing the download button produces exactly one download event between the press and the "Ready." status. The file is named `downloadStem('online', whole, false) + '.json'` (`index.html:787`): `hitopsr-online.json` for the whole instrument, otherwise `hitopsr-online-module.json`. Its text is what `hitop::write_module(hitop_module("hitopsr", scales = <ticked>), file)` writes. A smoke-spec assertion in `tests/smoke.spec.js` ticks two named scales and checks the file's `scales` and `items` against hand-listed display names and item numbers. The descriptor notice and the download hint describe the single `.json` save while this card is chosen.
- [ ] AC2: After that save the page shows one anchor labelled "Continue to the link builder" with `target="_blank"` and `rel="noopener"`. Its href is `https://jmgirard.github.io/hitop-form/link.html?c=<p>`, and base64url-decoding `<p>` and parsing it as JSON gives an object deep-equal to `{ instrument: "hitopsr", module: <the saved file's JSON> }`. A later tick change removes the anchor, and a second save replaces it, so at most one anchor is in the document. A smoke-spec assertion decodes the href, compares it to the saved file, and checks the removal and the replacement.
- [ ] AC3: After an online save followed by a Word build, no "Continue to the link builder" anchor is in the document, and the existing smoke assertions A1 to A11 stay green. A smoke-spec assertion shows the first part and the suite's run shows the second.
- [ ] AC4: `package.json` gains `"prose": "node tests/prose.mjs"`, and `smoke.yml` runs `npm run prose` after `npm ci` on every run of its existing triggers (every pull request, every push to main, the schedule and a manual run). Every new visitor-facing string is either static markup in `<body>` or written through a `.textContent =` or `setAttribute(` site listed in the ledger's `WRITERS`, and that run exits 0. `tests/plants.mjs` gains these plants: the online card absent; the saved file under a wrong name; the saved file's items out of order; the anchor's `c` carrying a module that differs from the saved file; the anchor's `c` lacking `instrument`; the href on a wrong base URL; the anchor without `target`; the anchor without `rel`; an anchor left in the document after a Word build. Each new assertion is failed by at least one of them, and `npm run plants`, run locally, reports every plant caught.
- [ ] AC5: The README's "What the page shows" section describes the fourth card, a new section "The online form" states what is saved and where the anchor leads, and the "What the downloads are named" table lists `hitopsr-online.json` and `hitopsr-online-module.json`. The Playwright suite and the prose step are green on the pull request's CI run.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T3
- AC5 → T4

## Tasks

- [ ] T1: The card in the `.formats` group (`index.html:505-518`), an `online` branch in `download()` (`index.html:1276-1454`) that calls `write_module(hitop_module("hitopsr", scales = ...), path)` in webR, saves the file through `saveFile()` (`index.html:1176-1186`), and builds the anchor with a base64url encoder of `JSON.stringify({ instrument, module })`.
- [ ] T2: The notice (`index.html:614-630`) and hint (`index.html:631-639`) copy for the online card; the anchor's lifecycle: removed on any tick change, replaced by a later save.
- [ ] T3: Smoke assertions for AC1 to AC3, the five plants, the ledger entries, `npm run prose`, and the `smoke.yml` step after `npm ci` (`smoke.yml:45`).
- [ ] T4: The README sections and table; run the suite and the prose step; open the PR and read its CI.

## Work log

- 2026-09-24: created by /milestone-plan.
- 2026-09-24: criteria audit ran in full mode (fresh [O] reader); its M126 repairs are written here (the stem rule follows `downloadStem`, the file is bound to `write_module()`, the notice and hint copy, the anchor lifecycle, one plant per assertion, the naming table). AC4's CI step was added at the gate and re-audited before writing.
- 2026-09-24: AC4 re-audited after the gate's CI-step change; four repairs applied before implement (the step runs on `smoke.yml`'s existing triggers, not "every push"; the string promise is bounded to static markup and the ledger's listed writer sites; nine plants replace five, one per attribute and per file check; the local `npm run plants` run is named as the evidence).
- 2026-09-24: plan gate chose adding the prose ledger's CI step here over promoting the whole candidate row, because widening the grep is harness work with no visitor-facing change; falsified by a new writer form on this page escaping the ledger in review.
- 2026-09-24: plan gate chose a saved `.json` plus an anchor over opening the link builder in the click handler, because the save is asynchronous under webR and a tab opened after it is blocked as a popup; falsified by a visitor report that the anchor is missed after the save.

## Decisions

## Review
