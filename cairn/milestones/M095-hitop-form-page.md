# M095: A static page renders a HiTOP-SR or HiTOP-BR form from the package's JSON export and saves each participant's responses to their device

- **Status:** review
- **Priority:** normal
- **Depends on:** M094
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP4
- **Resolves:** —
- **Surface tier:** user-facing — a participant-facing page and a researcher-facing link tool in a new public repository
- **Branch/PR:** `m095-hitop-form-page`

## Goal

Publish `jmgirard/hitop-form`, a static page that renders a HiTOP instrument or a HiTOP-SR module from the package's JSON export, walks a participant through it, and saves one response file to the participant's device, transmitting nothing.

## Scope

**In:** a new public repository served from GitHub Pages, plain HTML and JavaScript with no build step; the page fetches `https://jmgirard.github.io/hitop/downloads/<stem>.json`; a study link `?c=<base64url JSON>` carrying `instrument`, `study`, optional `participant`, and optional `module` (a descriptor as `write_module()` writes it); a `link.html` page that builds such a link from a pasted descriptor and study fields; the renderer, 15 items per page, a required-item check; a CSV save through a Blob and anchor click; version display; Playwright tests and CI mirrored from hitop-builder (D-062); a README.

**Out:** the PID-5 forms and the HSUM → the online-form candidate row. The package-side reader → M096. Storage adapters (Firestore, Apps Script, Supabase), their setup pages and consent text as configuration → the online-form candidate row.

## Acceptance criteria

- [x] AC1: A link naming `hitopsr` or `hitopbr` renders that export's `instructions.start`, then every element of its `items` array in export order, each with the export's `instructions.options` labels as one radio group; a Playwright test, for each of the two forms, reads the fetched JSON and asserts that every rendered item's text and every option label equal the JSON's and that the count of rendered items equals the array's length. (RB tripwire: ip-touching)
- [x] AC2: A link carrying a HiTOP-SR module descriptor as `write_module()` writes it renders the descriptor's `items` only, in `itemOrder` when present and otherwise in `items` order; a Playwright test uses two descriptors written by `write_module()` and committed under `tests/fixtures/`, a two-scale module with a shuffled `itemOrder` and the same module without one, and asserts the rendered item numbers equal each expected order.
- [x] AC3: The page shows 15 items on every page but the last, which shows the remaining items (fewer than 15 when the item count is not a multiple of 15), and refuses to advance while an item on the current page is unanswered, naming the item's position on the page; a test walks the full HiTOP-BR and the AC2 shuffled module (whose item count is not a multiple of 15), leaves the first, a middle and the last item of a full page blank in turn, and one item of each form's last page, reads each refusal, answers the item, and advances.
- [x] AC4: Finishing saves one CSV whose header is `study,participant,instrument,form_build,submitted` followed by the export's item names (the descriptor's items in rendered order when one is present), and whose one data row holds the study fields, `submitted` as an ISO-8601 UTC timestamp, and each item's chosen `value` as an integer; a test captures the saved Blob for the full HiTOP-BR, the full HiTOP-SR and the AC2 shuffled module, parses each, and the three files are committed under `tests/fixtures/` for the package's reader.
- [x] AC5: The page shows the export's `buildDate` and `packageVersion`, writes `buildDate` into `form_build`, and refuses an export whose `format` is not the string `"1.0"` with a message naming what it found; a test serves copies of the export with `format` altered, absent, and non-string, and reads each refusal.
- [x] AC6: Over three recorded walks (the full HiTOP-BR through save, the shuffled module through save, the altered-format refusal) the page makes no network request other than its own files and the one JSON fetch; a Playwright test records every request on each walk and asserts the set of request URLs.
- [x] AC7: `index.html` and `link.html` are reachable at the repository's Pages URL, workflow files run the Playwright tests on push and pull request and deploy the two pages on the default branch, and the README says how to make a study link, what the participant sees, where the file lands, and hands scoring to the package's modules article.

## Coverage

- AC1 → T3, T6
- AC2 → T4, T6
- AC3 → T3, T7
- AC4 → T5, T7
- AC5 → T2, T5, T8
- AC6 → T8
- AC7 → T1, T9

## Tasks

- [x] T1: Create `jmgirard/hitop-form` with `gh repo create` (public, LICENSE as hitop-builder's), enable Pages, add `package.json` and `package-lock.json` with `@playwright/test` pinned as hitop-builder's, and mirror `pages.yml`, `smoke.yml` and `tests/serve.mjs` from `../hitop-builder`.
- [x] T2: `index.html` and `form.js`: fetch the export and refuse a `format` other than `"1.0"`.
- [x] T3: `form.js`: render instructions, items and options, paginate, require answers.
- [x] T4: The study link: decode `?c=`, read the descriptor's `items` and `itemOrder`, and `link.html` that encodes a link from a pasted descriptor and study fields.
- [x] T5: Save: assemble the CSV, download through a Blob and anchor click, show `buildDate` and `packageVersion`.
- [x] T6: Playwright tests for AC1 and AC2, with the two descriptors written by `write_module()`.
- [x] T7: Playwright tests for AC3 and AC4; commit the three captured CSVs under `tests/fixtures/`.
- [x] T8: Playwright tests for AC5 and AC6, serving altered copies of the export locally (LESSONS 2026-08-25 on probing a served copy).
- [x] T9: README; confirm the Pages deploy serves both pages and the branch's CI run is green.

## Work log

- 2026-09-20: created by /milestone-plan.
- 2026-09-20: criteria audit ran in full mode on a fresh [O] reader over the six-form draft (see M094's work log); this file's criteria carry its fixes: the last page's item count, three probe positions, a descriptor without `itemOrder`, three `format` probes, three named network walks, the CI clause narrowed to the deliverable, committed CSV fixtures and an ISO-8601 `submitted` so M096 can rely on them.
- 2026-09-20: the audit's second pass (see M094's work log) fixed here: AC3 probes a partial last page through the module walk, and the renderer task split into T2 (fetch and format check) and T3 (render, paginate, require), tasks renumbered with Coverage.
- 2026-09-20: plan gate chose one encoded link parameter built by `link.html` over a descriptor fetched by URL because the link then needs no hosting and no second request; falsified by a mail client or LMS truncating links of about two kilobytes.
- 2026-09-20: plan gate chose a Playwright harness with the session creating the repository over no harness because nothing else catches a package export change breaking the page; falsified by Playwright's install being what turns CI red (D-062).
- 2026-09-20: plan chose fetching the export from the package's Pages site over vendoring a copy into hitop-form because one source carries the build date and checksum (D-016); falsified by a cross-origin refusal or a fetch slower than a participant waits.
- 2026-09-20: /milestone-implement started; branch `m095-hitop-form-page`; the served export answers with `access-control-allow-origin: *`, so the cross-origin fetch needs no copy.
- 2026-09-20: implement gate: the public repository is created at T1 rather than at review, so CI and the Pages deploy run during the work; a link carrying no `participant` makes the start screen ask for one (required) rather than leaving the column blank; items show their position on the form (1, 2, 3 in rendered order), as the printed Word form numbers a shuffled module, never the instrument number.
- 2026-09-20: T1 done in `/Users/jmgirard/github/hitop-form` (commit 897e884, pushed at T2 so the first Pages deploy has pages to stage): `gh repo create jmgirard/hitop-form --public`, Pages enabled with `build_type=workflow`, `@playwright/test` 1.56.1 in `package.json` and a fresh lockfile, LICENSE.md and `tests/serve.mjs` copied from hitop-builder, `pages.yml` staging `index.html link.html form.js LICENSE.md README.md`, and `tests.yml` (from `smoke.yml`) running `playwright test` on push, pull request, a Monday schedule and dispatch, the last two against the deployed page.
- 2026-09-20: T2–T5 done in one hitop-form commit (208ef62; one module, `form.js`, carries all four): `parseLink()` decodes `?c=` and checks the descriptor (instrument match, integer items, `itemOrder` a permutation); `checkExport()` refuses a `format` other than the string `"1.0"` naming what it found ("no format field", "a format that is not text (1)", `format "2.0"`); `planItems()` renders the descriptor's items in `itemOrder` else `items` order; pages of 15 with "Please answer item N on this page before continuing."; `buildCsv()` writes the `study,participant,instrument,form_build,submitted,<names>` header and one RFC 4180 row, `submitted` to the second in UTC; `link.html` encodes the link and refuses a descriptor whose instrument differs from the selected one. Fixtures `tests/fixtures/module-{plain,shuffled}.json` written by `write_module()` from `make-descriptors.R` (Distress-Dysphoria + Agoraphobia, 21 items, seed 95). Checked in the browser pane against the live export: BR start screen, page 1 of 3 with 15 items and four labels, refusal on item 3, three pages to the done screen; the shuffled module's page 1 in `itemOrder`; the three format probes; the link builder's output and two refusals. Not pushed yet: `pages.yml` stages README.md, so the first push follows the README stub at T6.
- 2026-09-20: T6 done (hitop-form 2nd commit after 208ef62): `tests/helpers.mjs` (target resolution with `FORM_TARGET`/`FORM_REQUIRE_TARGET`, link encoding, page reading and answering with a fixed answer pattern) and `tests/render.spec.js` with five named assertions R1–R5 over both instruments and both descriptors; 4 passed in 14 s locally against the live export; three plants each red by name (items reversed, `itemOrder` ignored, a label edited). README stub added and the branch pushed: first Pages deploy and Tests run started.
- 2026-09-20: T7 done (hitop-form 708b543): `tests/walk.spec.js` W1–W3 (page sizes, three probes on a full page in turn and one on the last page, each refusal read and the page shown not to advance) over the HiTOP-BR and the shuffled module; `tests/save.spec.js` S1–S5 (header, lead fields, integer values against the fixed answer pattern, fixture equality outside `form_build`/`submitted`, file name) over the three forms, with `WRITE_FIXTURES=1` writing `responses-{hitopbr,hitopsr,module-shuffled}.csv`, provenance in `tests/fixtures/README.md`, CRLF locked by `.gitattributes`. 5 passed; five plants red (page size 14, advance despite a blank, wrong position named, label written instead of value, `form_build` dropped). First CI: Tests 1m4s and Deploy to Pages 20s both green.
- 2026-09-20: T8 done (hitop-form 2520e23): `tests/guard.spec.js` G1–G4 (build date and package version on the start and done screens; `format` altered, absent and non-string each served in the live export's place through `page.route()` and each refusal read, plus the live copy accepted as the control) and `tests/network.spec.js` N1–N3 (every request recorded on the BR save, the module save and the refusal; the URL set equals the page, `form.js` and the export). 8 passed; three plants red (any string format accepted, a fetch to another host, build date dropped from the version line). Both pages gained `<link rel="icon" href="data:,">` so a real browser sends no favicon request either.
- 2026-09-20: the T7 push's Tests run failed in CI: `page.waitForEvent('download')` took the 15 s action timeout, and the 405-item HiTOP-SR walk took 18 s on the runner (9 s locally), so the wait for the file timed out before Finish. Fixed in `awaitDownload()` (110 s, inside the 120 s test budget); the retry hid nothing because both attempts hit the same wall.
- 2026-09-20: T9 done (hitop-form abfb0d9): README with the four sections AC7 names (making a link, what the participant sees, where the file lands, scoring handed to the modules article, plus development and license). Tests 1m11s and Deploy to Pages 18s green on that push; `index.html`, `link.html` and `README.md` answer 200 at the Pages URL. Full suite 17 passed locally.
- 2026-09-20: claim audit: 60 claims read, 3 corrected — hitop-form README.md (the package version is printed but not written to the file; the tests run on pull requests and pushes to `main`, not every push), tests/fixtures/README.md (no package reader exists yet); the reader ran over the hitop-form repository, since this branch adds no lines outside `cairn/`; link.html's order hint made conditional on the same pass; re-read confirmed all three (hitop-form 5cd69ca, CI green). Left as noted: the refusal names the item's index on the page (AC3's wording) while the item shows its position on the form, so page 2's "item 1 on this page" is the item labelled 16.
- 2026-09-20: all tasks checked; status → review.

## Decisions

## Review

- 2026-09-20 pass 1: reviewed `m095-hitop-form-page` (it adds the `hitop-form` launch entry and tracking) and hitop-form `main` at 5cd69ca, its tree clean and in step with its origin. hitop's `origin/main` e510aa09 is an ancestor of the branch, so there is nothing to merge. Fresh evidence: `npx playwright test` in hitop-form, 17 passed in 12.3 s against the live export, plus `gh run list` and `curl` for the deploy.
- AC1: PASS. `tests/render.spec.js` R1 to R3 over `hitopsr` and `hitopbr`: the start text equals `instructions.start`, the rendered count equals `items.length` (405 and 45), texts, numbers and positions equal the export's in order, and every item's labels and values equal `instructions.options`.
- AC2: PASS. R4 and R5 use `module-shuffled.json` and `module-plain.json`, written by `write_module()` through `tests/fixtures/make-descriptors.R` (two scales, 21 items, seed 95). The rendered numbers equal `itemOrder`, and equal `items` when it is absent.
- AC3: PASS. `tests/walk.spec.js` W1 to W3 over the HiTOP-BR (three full pages) and the shuffled module (15 then 6) assert each page's count. Items 1, 8 and 15 of page 1 are left blank in turn, then the last item of the last page. Each refusal reads "Please answer item k on this page before continuing." with the page number unchanged, then the item is answered and the walk advances to the done screen.
- AC4: PASS. `tests/save.spec.js` S1 to S5 over the HiTOP-BR, the HiTOP-SR and the shuffled module: the header is `study,participant,instrument,form_build,submitted` then the names in rendered order, one data row follows, `submitted` matches `YYYY-MM-DDThh:mm:ssZ` inside the test's clock window, every value is an integer equal to the fixed pattern, and the file equals the committed `responses-*.csv` outside `form_build` and `submitted`. The three CSVs and their provenance (`tests/fixtures/README.md`) are committed.
- AC5: PASS. `tests/guard.spec.js` G1 reads buildDate and packageVersion on the start and done screens. G2 to G4 serve `format` "2.0", absent and `1` in the live export's place, and each refusal names what was found with no Begin button and no items. The live copy is accepted as the control, and `form_build` equals `buildDate` in S2.
- AC6: PASS. `tests/network.spec.js` N1 to N3 record every request on the BR save, the module save and the altered-format refusal. The URL set equals {the page, `form.js`, the export} on each.
- AC7: PASS. `index.html`, `link.html` and `README.md` answer 200 at https://jmgirard.github.io/hitop-form/ (Pages build type `workflow`). `tests.yml` runs on `pull_request` and on `push` to `main`, and on Mondays and on dispatch against the deployed page. `pages.yml` deploys on push to `main`. The runs at 5cd69ca, Tests and Deploy to Pages, both succeeded. The README carries Make a study link, What the participant sees, Where the file lands, and Scoring, which links the modules article.
- Driving RR: none, so there is no projection to set beside a measurement.
- Gate: `cairn_validate` exit 0 (24 advisories, the standing dangling-id and references-staleness warnings). No principle changed, so `cairn_impact` is skipped. `document()` produced no diff. `check_pkgdown()` found no problems. NEWS: the branch changes no package behavior, and M094's entry already names a web form reading the export, so no entry is owed. `devtools::check()` 0 errors, 0 warnings, 0 notes (4 m 18 s).
- Fan-out (user-facing tier): [O] diff-bug 20 findings, [S] blame-history 0 (every divergence from the hitop-builder mirror is a commented adaptation, D-036/D-039/D-063/D-016/D-033 honored, the device-only save is architecture B's shared first phase per the ROADMAP row), [S] prior-review 3 (archived Review sections of M064, M089, M090, M092, M094 read; both PR-comment probes empty). Proposed dispositions below, decided at the gate.
- O1 the page fields a descriptor's recorded `items` and never rebuilds them from `scales` as `read_module()` does (D-039c), so a stale or edited descriptor collects columns the reader later refuses: follow-up, a note on the online-form row for M096's plan (the reader is where the tables live).
- O2 `checkModule` never checks the descriptor's own `format`: fix now, refuse a format other than "1.0".
- O3 `aria-live="polite"` on the whole `<main>` has every page turn read aloud: fix now, drop it (the alerts keep `role="alert"`).
- O4 focus is lost on every page turn and a refusal only scrolls: fix now, focus the page heading after a turn and the named item's first option after a refusal.
- O5 "nothing is sent anywhere" overstates: the link's study, participant and module ride in the URL the page host logs: fix now, the page and README say no answer is sent and that the link's contents reach the host.
- O6 a reload or back gesture loses every answer with no warning: fix now a `beforeunload` warning while answers are unsaved; persistence across reload is a follow-up on the online-form row.
- O7 a link with an empty or blank `participant` string suppresses the prompt and writes a blank column: fix now, a blank string counts as absent.
- O8 `stem`, `buildDate`, `packageVersion` and `package` are unguarded and can write the string `undefined` into the file: fix now, `checkExport` requires each as non-empty text.
- O9 (also P3) the refusal names the index on the page while items are labelled by form position: fix now, the message names both ("item 16, item 1 on this page"), AC3's position on the page kept.
- O10, O11 the CSV quoting path and the unicode link round trip are untested: fix now, a fourth save case with a study holding a comma, a quote and a non-ASCII character.
- O12 the network record drops every query string and stops at the assertion: fix now for the query half (strip it only from the page's own address); a `pagehide` beacon stays outside the record, follow-up on the online-form row.
- O13 `FORM_REQUIRE_TARGET` derives from the same expression as `FORM_TARGET`, so a renamed event empties both: fix now, require the target on every event that is not `push` or `pull_request`.
- O14 the Back button has no test: follow-up on the online-form row.
- O15 CSV formula injection through a study or participant string: reject, the file is read by R (M096), and a spreadsheet's cell execution is the spreadsheet's setting.
- O16 no encoding hint for `read.csv()` in the README: fix now, `fileEncoding = "UTF-8"` in the interim instruction.
- O17 the export's `stem` is never checked against the link's instrument: fix now, one comparison.
- O18 the Pages staging list is hand-maintained: reject, the three-file site is the design and AC7 reads the deployed URLs.
- O19 the link carries the whole descriptor: reject, `scales` is what O1 says the reader honors, 604 characters for the fixture.
- O20 an identical refusal twice in a row may not re-announce: reject, low, and O4's focus move gives the second press a signal.
- P1 pasting the instrument export into link.html's descriptor box gets a generic instrument-mismatch message (D-063's undiscriminated shape): fix now, link.html names a pasted export as the export.
- P2 link.html checks less than `form.js`'s `checkModule`, so a malformed descriptor fails at the participant: fix now, link.html imports and runs `checkModule`.
- P3 see O9.
- 2026-09-20 gate: Jeff accepted the triage above. The 16 fix-now items landed in one hitop-form commit after 5cd69ca: `checkModule` (now exported and run by link.html) refuses a `format` other than "1.0" and names a pasted export as the export; `checkExport` requires `stem`, `buildDate`, `packageVersion` and `package` as text and the stem equal to the link's instrument; a blank `participant` counts as absent; the refusal reads "Please answer item 16 (item 1 on this page)"; `aria-live` dropped from `<main>`, each screen's heading takes focus and a refusal focuses the named item's first option; a `beforeunload` warning while answers are unsaved; "No answer is sent" on both screens and the README, which also states that the link's contents reach the host's logs and gives `fileEncoding = "UTF-8"`; the network record keeps query strings except on the page's own address; `FORM_REQUIRE_TARGET` is set on every event that is not `push` or `pull_request`. Tests: a fourth save case with a comma, a quote and a non-ASCII study (S6 asserts the quoted bytes), four guard tests (stem mismatch, missing buildDate, descriptor format "2.0", blank participant), focus asserted in the walk. 22 passed in 14.2 s locally. link.html checked in the browser pane: a pasted export, non-integer items, and a valid descriptor. hitop-form CI at 21a1d1c: Tests and Deploy to Pages both success.
