# M095: A static page renders a HiTOP-SR or HiTOP-BR form from the package's JSON export and saves each participant's responses to their device

- **Status:** in-progress
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

- [ ] AC1: A link naming `hitopsr` or `hitopbr` renders that export's `instructions.start`, then every element of its `items` array in export order, each with the export's `instructions.options` labels as one radio group; a Playwright test, for each of the two forms, reads the fetched JSON and asserts that every rendered item's text and every option label equal the JSON's and that the count of rendered items equals the array's length. (RB tripwire: ip-touching)
- [ ] AC2: A link carrying a HiTOP-SR module descriptor as `write_module()` writes it renders the descriptor's `items` only, in `itemOrder` when present and otherwise in `items` order; a Playwright test uses two descriptors written by `write_module()` and committed under `tests/fixtures/`, a two-scale module with a shuffled `itemOrder` and the same module without one, and asserts the rendered item numbers equal each expected order.
- [ ] AC3: The page shows 15 items on every page but the last, which shows the remaining items (fewer than 15 when the item count is not a multiple of 15), and refuses to advance while an item on the current page is unanswered, naming the item's position on the page; a test walks the full HiTOP-BR and the AC2 shuffled module (whose item count is not a multiple of 15), leaves the first, a middle and the last item of a full page blank in turn, and one item of each form's last page, reads each refusal, answers the item, and advances.
- [ ] AC4: Finishing saves one CSV whose header is `study,participant,instrument,form_build,submitted` followed by the export's item names (the descriptor's items in rendered order when one is present), and whose one data row holds the study fields, `submitted` as an ISO-8601 UTC timestamp, and each item's chosen `value` as an integer; a test captures the saved Blob for the full HiTOP-BR, the full HiTOP-SR and the AC2 shuffled module, parses each, and the three files are committed under `tests/fixtures/` for the package's reader.
- [ ] AC5: The page shows the export's `buildDate` and `packageVersion`, writes `buildDate` into `form_build`, and refuses an export whose `format` is not the string `"1.0"` with a message naming what it found; a test serves copies of the export with `format` altered, absent, and non-string, and reads each refusal.
- [ ] AC6: Over three recorded walks (the full HiTOP-BR through save, the shuffled module through save, the altered-format refusal) the page makes no network request other than its own files and the one JSON fetch; a Playwright test records every request on each walk and asserts the set of request URLs.
- [ ] AC7: `index.html` and `link.html` are reachable at the repository's Pages URL, workflow files run the Playwright tests on push and pull request and deploy the two pages on the default branch, and the README says how to make a study link, what the participant sees, where the file lands, and hands scoring to the package's modules article.

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
- [ ] T2: `index.html` and `form.js`: fetch the export and refuse a `format` other than `"1.0"`.
- [ ] T3: `form.js`: render instructions, items and options, paginate, require answers.
- [ ] T4: The study link: decode `?c=`, read the descriptor's `items` and `itemOrder`, and `link.html` that encodes a link from a pasted descriptor and study fields.
- [ ] T5: Save: assemble the CSV, download through a Blob and anchor click, show `buildDate` and `packageVersion`.
- [ ] T6: Playwright tests for AC1 and AC2, with the two descriptors written by `write_module()`.
- [ ] T7: Playwright tests for AC3 and AC4; commit the three captured CSVs under `tests/fixtures/`.
- [ ] T8: Playwright tests for AC5 and AC6, serving altered copies of the export locally (LESSONS 2026-08-25 on probing a served copy).
- [ ] T9: README; confirm the Pages deploy serves both pages and the branch's CI run is green.

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

## Decisions

## Review
