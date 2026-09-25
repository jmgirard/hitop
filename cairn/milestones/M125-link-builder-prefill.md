# M125: hitop-form's `link.html` fills its fields from a study link's `c` parameter, so a link can be edited and another page can hand a module over

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — a researcher-facing page on the deployed hitop-form site
- **Branch/PR:** —

## Goal

Make hitop-form's link builder read the `?c=` parameter the form page already reads, fill its controls from it, refuse one it cannot read by name, and show its place in the flow with links to the module builder and the online-collection tutorial.

## Scope

**In:** `link.html` and `form.js` in `jmgirard/hitop-form`: on load, decode a `c` parameter with `decodeConfig()`, fill every control it names, show the chosen store kind's field group, and write a refusal into `#err` for a parameter that cannot be read or names an instrument the page does not offer. A three-step list above the form. A link to the module builder in the module field's hint and a link to the tutorial. The README paragraph. Playwright tests.

**Out:** the builder's "Online form" card → M126. The package site's strip, menu entry and step list → M124. A module for an instrument other than the HiTOP-SR → DESIGN Known issue 11 stands. Re-normalising a store or completion URL on prefill → not needed, since the values come from links the page wrote.

## Acceptance criteria

- [ ] AC1: Opening `link.html?c=<p>` fills each control from the config that `<p>` encodes, where `<p>` is `encodeConfig(config)`. Pressing "Make the link" then yields a link whose `c` decodes to a config deep-equal to the one opened, and the chosen store kind's field group is shown with the other hidden. A test in `tests/link.spec.js` shows this once per store choice: no `store` field, a `webhook` store, and a `supabase` store. One run sets `participant`; another sets `prolific: true` with no participant. Each run sets `module`, `shuffle`, `complete` and `completeSaved`. Every URL and text value in the opened config is already in the form `link.html` writes.
- [ ] AC2: A `c` whose config carries only `instrument` selects that instrument, and every other input's `value` and every checkbox's `checked` equal the same control's on a load with no `c`. A config carrying `instrument` and `module` also fills the module textarea with JSON text whose `JSON.parse` is deep-equal to the descriptor. Two tests in `tests/link.spec.js` show this.
- [ ] AC3: A `c` that `decodeConfig` cannot decode, one that decodes to a value that is not a plain object, and one whose `instrument` is not one of the select's five values each write a message containing the text `c parameter` into `#err` and leave every control at its no-`c` value. A load with no `c` leaves `#err` empty. Tests in `tests/link.spec.js` show this for seven loads: a string that is not base64url, a base64url string that is not JSON, a JSON array, JSON `null`, JSON `"x"`, an object with `instrument` `"hitophsum"`, and no `c`.
- [ ] AC4: Above the form sits an ordered list of three items whose text contains, in document order, "Choose the instrument", "where the responses go" and "Make the link". The module field's hint carries a link with href `https://jmgirard.github.io/hitop-builder/`, and the page carries a link with href `https://jmgirard.github.io/hitop/articles/online-collection.html`. A test in `tests/link.spec.js` asserts the two hrefs and the three items' order.
- [ ] AC5: A load of `link.html` with AC1's Supabase config requests nothing but `link.html` and `form.js` from navigation to the first network idle. A test in `tests/network.spec.js` records every request over that window and compares URLs with the query dropped, as its `recorded()` helper does.
- [ ] AC6: The Playwright suite is green locally and on the pull request's CI run. `README.md` gains a paragraph under "Make a study link" stating that a study link's `c` parameter opened on `link.html` fills the fields for editing, and that a page can hand a module over that way.

## Coverage

- AC1 → T1, T3
- AC2 → T1, T3
- AC3 → T1, T3
- AC4 → T2, T3
- AC5 → T1, T3
- AC6 → T4

## Tasks

- [ ] T1: In `link.html` (`link.html:151-160`, the module script) read `c` from `location.search` on load, decode it with `decodeConfig()` (`form.js:62`), fill each control, call the kind-group toggle (`link.html:170-175`) after setting the store kind, and write the AC3 refusals into `#err`. The instrument check reads the select's own options.
- [ ] T2: Add the three-step list above the form, the builder link in the module hint (`link.html:84-87`), the tutorial link, and an up-nav link to the package site in the builder's shape (`hitop-builder/index.html:437-440`).
- [ ] T3: Tests: the AC1 round trips, the AC2 comparisons against a no-`c` load, the seven AC3 loads, the AC4 hrefs and order in `tests/link.spec.js`, and the AC5 request window in `tests/network.spec.js`.
- [ ] T4: The README paragraph; run the suite; open the PR and read its CI.

## Work log

- 2026-09-24: created by /milestone-plan.
- 2026-09-24: criteria audit ran in full mode (fresh [O] reader, user-facing tier) over the three drafts; 36 findings, 31 repaired before writing (store kinds, participant beside Prolific, pre-normalised values, hidden kind groups, checkbox state, unknown instrument, seven malformed loads, exact list phrases, the network window, the README's tense), five accepted as test-run gates.
- 2026-09-24: plan gate chose prefilled hand-off links over merging the link builder into the module builder because every link build would then wait on the builder's webR boot and the five-instrument page would collapse to the HiTOP-SR; falsified by a researcher report that two pages with hand-offs still read as disjointed.
- 2026-09-24: plan gate chose prefill over cross-links with no parameter reading because the pasted descriptor is the step researchers lose; falsified by a report that the prefilled link fails where the paste worked.

## Decisions

## Review
