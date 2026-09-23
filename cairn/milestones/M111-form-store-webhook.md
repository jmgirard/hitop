# M111: hitop-form sends each participant's responses to an endpoint named in the study link

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — a deployed page participants fill in and researchers configure
- **Branch/PR:** `m111-form-store-webhook` (hitop, `cairn/` only); `store-webhook` in jmgirard/hitop-form

## Goal

Extend `jmgirard/hitop-form` so a study link can name a store, and the page then posts the finished responses to it and saves them to the participant's device only when the send cannot be confirmed.

## Scope

**In:** one store kind, `webhook`: an HTTPS endpoint that accepts a POST of one JSON row and answers `{"ok":true}`. A Google Apps Script web app appending to a Google Sheet is the documented instance. The link builder's store field and start-screen disclosure. The device save as the fallback. Playwright tests against a local recording endpoint. One hand run against a deployed script, its sheet export committed as a fixture. Participant-facing text stays outside instrument content (IP1): item text, options and instructions are untouched.

**Out:** the Supabase kind → M112. Firestore, consent text, the HSUM export, the M095 open points → the online-form candidate row. Reading a multi-row export in the package and the walkthrough article → M113. In-browser encryption of each row → its own candidate row.

## Acceptance criteria

- [ ] AC1: A study link whose `store` is `{ "kind": "webhook", "url": <URL> }` makes the page send one POST to that URL when Finish is pressed. The body is one JSON object. Its keys are `study`, `participant`, `instrument`, `form_build`, `submitted`, then one key per item in the order the page showed them. Its values equal the CSV fixture of the same walk in every field but `submitted` (a UTC stamp taken during the test) and `form_build` (the export's `buildDate`). Item values are JSON integers. The request is a CORS simple request: method POST, `Content-Type: text/plain;charset=utf-8`, and no header outside the CORS-safelisted set. Finish is disabled from its first press until the outcome screen shows. Tests: a recording endpoint added to `tests/serve.mjs` runs on a second origin, records method, headers and body of every request, answers OPTIONS with CORS headers, and offers a `/redirect` path answering 302 to a record path on a third origin (another port) that answers with `Access-Control-Allow-Origin: *`. The tests walk the HiTOP-BR and the shuffled HiTOP-SR module fixture, once each through the record path and once through `/redirect`. They compare the recorded body field by field, assert that no OPTIONS request was recorded, and on one walk press Finish twice in one gesture (a `dblclick`, or a second forced click before the response arrives) and count one POST.
- [ ] AC2: The page treats a send as confirmed only when the response status is 2xx and the body parses as JSON with `ok` equal to `true`. On a confirmed send the page saves no file, and its final screen says the responses were sent to the study team. On any other outcome the page saves the CSV to the device as it does today, and its final screen says the send could not be confirmed and names the saved file. No failure screen states that no answer was sent. One test per outcome: a 200 with `{"ok":true}` (no download event), a 200 with an HTML body, a 404, a 500, a refused connection on an unused port, and a `/hang/` endpoint against the page's 30-second limit.
- [ ] AC3: Without `store` in the link, tests N1 to N3 hold unchanged. With a `store` that does not redirect, the recorded request set of a walk through Finish equals { the page, form.js, the export, the store URL }. Through `/redirect` it equals that set plus the redirect target. No request to the store URL is recorded before Finish is pressed. A test records the set at the last page before Finish and again after.
- [ ] AC4: `parseLink()` accepts a `url` that is `https:`, or `http:` with host `127.0.0.1` or `localhost`. It refuses every `url` outside that rule, a `store` that is `null`, an array or a string, and one whose `kind` is not a kind the page knows. Each refusal names the fault. Refused forms probed: a missing `url`, a non-string, unparsable text, `http:` to another host, `http://localhost.example.com`, `http://127.0.0.1.example.com`, a `javascript:` URL and `data:text/plain,x`. Accepted forms probed: `https:`, `http://127.0.0.1:<port>` and `http://localhost:<port>`. link.html refuses a missing or unaccepted URL before building a link. One guard test per listed form in `tests/guard.spec.js`, and one builder test per builder fault in `tests/link.spec.js`.
- [ ] AC5: link.html has a "Send responses to" URL field. A link built with it set opens a form whose Finish posts to that URL (link spec). With a store, the start screen names the store URL's host as where the responses go, and says the file is saved to the device if the send cannot be confirmed. Without a store, the start screen keeps today's wording. A search of the text shown to users in `index.html`, `link.html` and `form.js` (code comments excluded) and of the README prose for `sent anywhere`, `No answer`, `sent from this page`, `stored or sent`, `device` and `leaves the page` finds every hit inside text shown only without a store, or inside README prose that says it applies without a store. A render test asserts the two start-screen wordings.
- [ ] AC6: The README gains a "Send responses to a Google Sheet" section with the `doPost` code, the deployment steps and the CSV download step. The `doPost` code formats every cell as text before writing and answers `{"ok":true}` as JSON. One hand run against a web app deployed from that code: two HiTOP-BR walks, participants `=1+1` and `007`, append two rows. The sheet's CSV download then equals, field for field, the two bodies the browser's network panel shows were posted, `submitted` and both participant codes included. That download is committed as `tests/fixtures/sheet-hitopbr.csv` with a provenance row in `tests/fixtures/README.md`.
- [ ] AC7: `npx playwright test` passes against the checkout. The send tests start their own recording endpoint whether or not `FORM_TARGET` is set. The hitop-form pull request's Tests workflow is green. A dispatched run against the deployed page passes after the merge, with the send tests either granting the browser context's local-network permission or skipping under `FORM_TARGET` with the reason printed (the first dispatched run settles which). The hitop repository's change is under `cairn/` only.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T4
- AC4 → T1
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1: `checkStore()` in form.js, called by `parseLink()` (form.js:54) and by link.html's builder. Guard tests per AC4's listed forms, builder tests per builder fault.
- [x] T2: The recording endpoint in `tests/serve.mjs` on a second port: record, `/redirect`, an HTML-200 path, a status path, OPTIONS with CORS headers, a request log the tests read. A helper in `tests/helpers.mjs` starts it whatever the target (AC7).
- [x] T3: `sendResponses()` in form.js: `fetch` POST with `text/plain`, `redirect: 'follow'`, an `AbortController` at 30 seconds, the confirmation rule, Finish disabled, the two outcome screens, the device fallback through `saveFile()` (form.js:235). `tests/send.spec.js` covers AC1 and AC2. The download promise is created before the walk with its own timeout (LESSONS, M095).
- [ ] T4: `tests/network.spec.js` gains the with-store walks of AC3.
- [ ] T5: The link.html field, the start-screen wordings, the text sweep of AC5, and the render and link tests.
- [ ] T6: The README section and its `doPost` code. Deploy it, run the two walks, compare the download with the posted bodies, commit the fixture and its provenance row. Work-log line: the date, the deployment version, and the comparison result.
- [ ] T7: Open the hitop-form pull request at implement and merge it at review (the merge publishes the page, as M098 did). Dispatch a run against the deployed page after the merge. In hitop, commit `cairn/` only.

## Work log

- 2026-09-23: created by /milestone-plan. Promoted from the online-form candidate row (lineage M093–M099); the row stays until completion and is annotated.
- 2026-09-23: criteria audit ran in full mode on a fresh [O] reader over the M111–M113 drafts: 20 findings, all fixed in the wording before the gate (loopback exception so the local tests can post, second-origin endpoint so the simple-request and redirect claims are probed, confirmation by acknowledgment body, Finish disabled, six outcome probes, builder faults split from guard faults, text sweep by search, text-formatted cells and the `=1+1` walk, the sheet export kept as a fixture, endpoint under `FORM_TARGET`). None posed as a question.
- 2026-09-23: plan gate chose send-only on a confirmed send over always saving a device copy because a confirmed send leaves no identifiable file in a participant's downloads folder; falsified by a study reporting rows lost after a confirmed send.
- 2026-09-23: plan chose a `text/plain` simple request over a JSON content type because an Apps Script web app answers no CORS preflight; falsified by Apps Script answering OPTIONS.
- 2026-09-23: plan chose confirmation by a 2xx plus an `{"ok":true}` body over a 2xx alone because an Apps Script web app answers 200 with an HTML page when `doPost` throws; falsified by Apps Script returning error statuses.
- 2026-09-23: plan chose an `http:` loopback exception in the URL guard over an `https:`-only guard because the recording endpoint is a local server; falsified by a Playwright route shown to intercept a cross-origin `https:` POST and its preflight.
- 2026-09-23: plan chose the store inside the encoded link over a store fetched from a descriptor URL because M095 settled one encoded link parameter; falsified by links a mail client truncates in the field.
- 2026-09-23: re-audit of the reworded criteria by the same fresh [O] reader: eleven findings across M111–M113, six on this file (a third-origin redirect target, the double press as one gesture, the request set under a redirect, near-miss loopback hosts and unlisted schemes, two missed device-save sentences and code comments in the sweep, Chromium's local-network rule for the dispatched run). All fixed in the wording after the plan commit; none reopened a gate choice.
- 2026-09-23: implement started by /milestone-implement. Branches cut from the pushed default branches: `m111-form-store-webhook` in hitop, `store-webhook` in hitop-form. Jeff had the stray untracked `qtest.txt` in the hitop tree deleted before branching.
- 2026-09-23: implement gate. Jeff deploys the Apps Script web app from the README code and downloads the sheet's CSV himself, because the session holds no Google account. The session runs the two walks against his `/exec` URL. Under `FORM_TARGET`, the send tests first ask Playwright to grant the local-network permission. If that call is refused, they skip with the reason printed (AC7).
- 2026-09-23: T1 done (hitop-form `store-webhook`). `checkStore()` and `checkStoreUrl()` in form.js, called by `parseLink()` and by link.html's new "Send responses to" field. 15 guard tests (12 refused forms, 3 accepted) and 4 builder tests. A plant that accepted every url turned 7 of them red. The link.html hint avoids the word "device" because AC5's sweep bars it from text shown with a store.
- 2026-09-23: T2 done. `serveStore()` in tests/serve.mjs starts the endpoint and a third-origin twin. It records every request and answers `/record`, `/redirect`, `/html`, `/status/<n>`, `/hang/` and OPTIONS. `useStore()`, `allowLocalStore()` and `webhook()` in tests/helpers.mjs. Exercised from Node: the 302 lands on the twin as a GET, and the body is recorded at `/redirect`. Playwright 1.56 maps `local-network-access` to Chromium's permission, so the AC7 grant is a known name.
- 2026-09-23: T3 done. `buildRow()` and `sendResponses()` in form.js, the sent and unconfirmed screens, the with-store start-screen wording, and `tests/send.spec.js` (9 tests: four fixture walks through `/record` and `/redirect`, five unconfirmed outcomes). `parseCsv()` moved from save.spec.js to helpers.mjs. Three plants went red: no double-press guard (two POSTs), a 2xx alone confirming (no download on the HTML body), and an `application/json` content type. Full suite 63 passed.

## Decisions

## Review
