# M112: hitop-form inserts each participant's responses into a Supabase table named in the study link

- **Status:** review
- **Priority:** normal
- **Depends on:** M111
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — a store kind researchers configure on the deployed page
- **Branch/PR:** hitop `m112-form-store-supabase` (cairn/ only), PR #123 https://github.com/jmgirard/hitop/pull/123; hitop-form `store-supabase`, PR #3 https://github.com/jmgirard/hitop-form/pull/3

## Goal

Add a second store kind to `jmgirard/hitop-form`, `supabase`, that inserts the finished responses as one row of a table the researcher owns, with the link builder writing the SQL that creates that table for the chosen form.

## Scope

**In:** the `supabase` kind on M111's send path (headers, confirmation on status, the same JSON row). The builder's three store fields (project URL, key, table name) and the SQL it shows: table, row-level security, an insert-only policy for `anon`. README setup and export steps, including the free-tier pause. One hand run against a real project, its export committed as a fixture. Participant-facing text stays outside instrument content (IP1); the SQL fixture is hand-written, never captured from the builder (IP2).

**Out:** Firestore → the online-form candidate row. Reading the export in the package → M113. A relay holding a REDCap token → ruled out by `cairn/references/online-collection.md` (a static page cannot hold one).

## Acceptance criteria

- [x] AC1: A study link whose `store` is `{ "kind": "supabase", "url": <project URL>, "key": <key>, "table": <name> }` makes the page send one POST to `<url>/rest/v1/<table>`, with trailing slashes removed from `<url>`, when Finish is pressed. The request headers are `apikey: <key>`, `Content-Type: application/json`, `Prefer: return=minimal`, and `Authorization: Bearer <key>` only when the key has the three dot-separated segments of a JWT. The body is the JSON object M111's AC1 specifies. A send is confirmed on a 2xx status alone. Tests against M111's recording endpoint: one HiTOP-BR walk with a JWT-shaped key, one with an `sb_publishable_` key, and one with a project URL ending in a slash. They compare the URL, the headers with these literals, and the body with the fixture as M111's AC1 does, and assert that the endpoint recorded and answered one OPTIONS request per walk.
- [x] AC2: For the chosen instrument or pasted module, link.html shows SQL that creates the table named in its field, the name double-quoted. The table has `study`, `participant`, `instrument`, `form_build` and `submitted` as `text`, then one `integer` column per item in the order the page will show them. The SQL enables row-level security, grants insert on the table to `anon`, and adds one policy `for insert to anon with check (true)`. A test compares the shown SQL for the HiTOP-BR and for the shuffled module fixture with hand-written fixtures under `tests/fixtures/`, whose provenance row names the Supabase policy documentation page and the package's item names.
- [x] AC3: `parseLink()` refuses a `supabase` store whose `url` fails M111's AC4 rule, whose `key` is missing, empty or not a string, or whose `table` does not match `^[a-z_][a-z0-9_]{0,62}$`. Each refusal names the fault. link.html refuses the same three faults before building a link. Table forms probed: `Responses`, `1abc`, `a-b`, the empty string and a 64-character name. One guard test per listed form and one builder test per fault.
- [x] AC4: The confirmed walks of AC1 fire no download event and show the sent screen. A walk against an endpoint answering 401 and a walk against a refused connection each save the CSV to the device and show the failure screen (two tests).
- [x] AC5: The README gains a "Send responses to Supabase" section: creating a project, pasting the builder's SQL into the SQL editor, finding the key, the free project's pause after one idle week, and the Table Editor's CSV export. One hand run against a project whose table came from that SQL: one HiTOP-BR walk with the project's `sb_publishable_` key inserts one row, and the CSV export equals, field for field, the body the browser's network panel shows was posted. If the project still issues a legacy anon JWT, a second walk with it inserts a second row. With the same key, a REST select returns no rows, and an update and a delete change no rows. The export is committed as `tests/fixtures/supabase-hitopbr.csv` with a provenance row.
- [x] AC6: `npx playwright test` passes against the checkout. The hitop-form pull request's Tests workflow is green, and a dispatched run against the deployed page passes after the merge under M111's AC7 local-network rule. The hitop repository's change is under `cairn/` only.

## Coverage

- AC1 → T3
- AC2 → T2
- AC3 → T1
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1: Extend `checkStore()` with the `supabase` fields; guard and builder tests per AC3.
- [x] T2: `storeSql()` in link.html for the chosen instrument or module, the hand-written SQL fixtures with their provenance row, and the comparison test.
- [x] T3: The `supabase` branch of `sendResponses()`: URL, headers, the JWT test for `Authorization`, confirmation on status. The recorder answers OPTIONS with `Access-Control-Max-Age: 0` so no preflight is cached across walks. Send tests with both key shapes, the OPTIONS assertion, and the with-store network walk.
- [x] T4: The 401 and refused-connection tests.
- [x] T5: The README section. Create the project, run the walk, compare the export with the posted body, run the select, update and delete probes, commit the fixture and its provenance row. Work-log line: the date, the project region, and the probe results.
- [x] T6: Open the hitop-form pull request at implement and merge it at review. Dispatch a run against the deployed page after the merge. In hitop, commit `cairn/` only.

## Work log

- 2026-09-23: created by /milestone-plan. Promoted from the online-form candidate row (lineage M093); planned with M111 and M113.
- 2026-09-23: criteria audit ran in full mode on a fresh [O] reader (shared with M111): findings on this file were the `Authorization` header with a non-JWT key, the SQL fixture captured from the builder (IP2), the unverifiable "nothing else" in the policy claim, reserved-word table names, and the header-versus-fixture comparison. All fixed in the wording; none posed as a question.
- 2026-09-23: plan chose one `integer` column per item over a single `jsonb` column because the Table Editor's CSV export is then one tidy row per participant; falsified by a form whose column count exceeds Postgres's limit of 1,600.
- 2026-09-23: plan chose builder-generated SQL over a README template because a HiTOP-SR table has 410 columns nobody should type; falsified by a researcher needing a table shape the builder cannot express.
- 2026-09-23: re-audit by the same fresh [O] reader: findings on this file were the hand run's key type, a trailing slash in the project URL, a cached preflight, one exemplar per table-name fault, and AC4 claiming all six of M111's outcomes on two probes. All fixed in the wording after the plan commit.
- 2026-09-23: implement gate. Jeff chose a "Send responses to" kind selector in the builder (file, web address, Supabase table), the builder fetching the export for the SQL's item names, the SQL shown under the built link after "Make the link", and a split hand run: Jeff creates the project and runs the SQL, the session runs the walk and the REST probes, Jeff exports the CSV.
- 2026-09-23: T1 and T2 done in hitop-form (branch `store-supabase`). `checkStore()` takes the `supabase` kind with `key` and `table` (`TABLE_NAME` regex); G8 in guard.spec (10 refused forms, 4 accepted); link.html gained a "Send responses to" kind selector, the three Supabase fields and the SQL block; L6 (three builder faults) and L7 (SQL against the two fixtures) in link.spec. `storeSql()` lives in form.js rather than link.html so the test-facing logic stays in the one module (minor task-wording change). The old "empty address builds a link with no store" test became a refusal under the selector. A planted `with check (false)` turned L7 red on both fixtures. 57 of 57 in the two specs.
- 2026-09-23: T3 and T4 done (hitop-form 6f98e19). `sendRequest()` builds the insert URL (trailing slashes stripped) and headers, `isJwtShaped()` gates `Authorization`; a supabase send is confirmed on `res.ok` alone. The recorder lists the four headers in `Access-Control-Allow-Headers` (a wildcard does not cover `Authorization`), sends `Access-Control-Max-Age: 0`, and answers `…/rest/v1/<table>` with 201 and no body, or the status a `status_<nnn>` table names. T9 (three walks: JWT-shaped key, publishable key, URL ending in a slash; one OPTIONS each), T10 (401, refused connection) in send.spec; N6 in network.spec (the preflight and the POST share the one address, so the URL set is unchanged). Plants: an always-sent `Authorization` and an unstripped slash each turned their walk red. Full suite 96 of 96 (was 70).
- 2026-09-23: T5's README section written (hitop-form 598fc62): project, the three fields and where the dashboard shows them, the SQL and what it grants, the one-week pause, the Table Editor export, the open-insert risk. The hand run waits on Jeff's project URL and key.
- 2026-09-23: hitop-form PR #3 opened from `store-supabase` (T6's first half). Status set to blocked: the AC5 hand run needs a Supabase project Jeff creates, its table made from the builder's SQL, and its project URL and publishable key given in chat; then the CSV export from the Table Editor. Left for the resume: the walk and the REST probes, the fixture and its provenance row, the claim audit over the hitop-form diff, and the T5 work-log line.
- 2026-09-23: hand run, part one. Jeff's project (URL `https://etjfuxonozeulotfbkrp.supabase.co`, table `hitopbr_test` made from the builder's SQL, publishable key). A column probe found all 50 columns and no `id`. One HiTOP-BR walk from the checkout (participant `p001`, `submitted` 2026-09-23T21:01:51Z) posted to `/rest/v1/hitopbr_test` with the four headers and was answered 201. With the same key: select counted 0 rows (`content-range: */0`), an update and a delete each returned no affected rows, and the count stayed 0. The posted body is kept in the scratchpad for the export comparison. Jeff's first pasted URL ended in `/rest/v1/`, which the guard accepted and the send would have doubled; `checkStore()` now drops that suffix (hitop-form, one guard, one builder and one send test added). Waiting on: the row count in the Table Editor, whether a legacy anon key exists, and the CSV export.
- 2026-09-23: T5 done. Hand run, part two: the project had a legacy anon JWT; a second walk with it (participant `p002`, `submitted` 2026-09-23T21:03:49Z) sent `apikey` and `Authorization: Bearer` and was answered 201; select, update and delete with that key touched 0 rows. Jeff confirmed two rows in the Table Editor and exported the CSV; it equals the two posted bodies in 100 of 100 fields. Committed as `tests/fixtures/supabase-hitopbr.csv` with its provenance row and T11 (hitop-form 2 commits after 6eee5e1). The export warns that the table has no primary key; the README says the SQL adds none on purpose. Project region: not recorded by the session (the dashboard shows it to Jeff only). Suite 100 of 100.
- 2026-09-23: claim audit: 88 claims read, 12 corrected — form.js, link.html, README.md, tests/send.spec.js, tests/link.spec.js, tests/guard.spec.js, tests/serve.mjs (hitop-form f06e1d6; the reader's one re-read found all twelve correct). A live probe backed the README's unknown-key claim: a POST with a key the table lacks answered 400 PGRST204.
- 2026-09-23: step-7 approval: m112-form-store-supabase approved for merge (with hitop-form store-supabase, PR #3), after the 16 review fixes.
- 2026-09-23: review step 8, first pass: PR #3 green on 034bd84 but its merge needed the session's working directory inside hitop-form (the merge guard reads the approval from the session cwd's repo); hitop PR #123 opened and its watch hit the harness ceiling with 4 of 8 checks pending; watcher stopped. Two docs-only commits on the branch (the PR record, this checkpoint) were left unpushed and went with the squash; their content is restated here.
- 2026-09-23: resume: PR #123 merged 2026-09-23 (fa809196) after 8 of 8 green, marker written; session moved into hitop-form; PR #3 merged (c15e149); deployed-page run dispatched and green; re-entering at step 9.
- 2026-09-23: T6's implement half done (PR #3 open, head f06e1d6, its Tests run pending at this write); the merge and the dispatched run are review's by the task's own text. Status set to review.

## Decisions

- 2026-09-23 (review, F4): `checkStore()` normalizes a supabase store's `url` to the project origin, dropping a pasted `/rest/v1` suffix and trailing slashes and refusing any other path, query or fragment. AC1's `<url>` is that checked url; the send appends `/rest/v1/<table>` to it. Chosen because the dashboard shows the REST URL with the suffix and Jeff's first paste carried it.

## Review

Fresh run 2026-09-23 on hitop-form `store-supabase` at f06e1d6, hitop `m112-form-store-supabase` (cairn/ only). `npx playwright test`: 100 of 100 passed (49.7 s).

- AC1: send.spec T9, four HiTOP-BR walks against the recording endpoint (JWT-shaped key, `sb_publishable_` key, project URL ending in a slash, and one ending in `/rest/v1/`): each one POST to `/project/rest/v1/hitopbr_responses`, `apikey`, `content-type: application/json` and `prefer: return=minimal` asserted as literals, `authorization: Bearer <key>` asserted present for the JWT shape and absent otherwise, the body compared with `responses-hitopbr.csv` field by field as M111's T1 does, and exactly one OPTIONS recorded per walk to the insert address with `access-control-request-method: POST`. Confirmation on status alone: the recorder answers the insert 201 with no body and the walks reach the sent screen. Code read: form.js lines 358–360 build the URL with `replace(/\/+$/, '')` and the three headers, `isJwtShaped()` gating the fourth.
- AC2: link.spec L7, the shown SQL for the HiTOP-BR (`hitopbr_responses`) and the shuffled module (`module_responses`) equals `tests/fixtures/supabase-hitopbr.sql` and `supabase-module-shuffled.sql` byte for byte. The fixtures read: five `text` columns in the named order, then 45 and 21 `integer` columns (the module's in `itemOrder`), `enable row level security`, `grant insert … to anon`, one policy `for insert to anon with check (true)`, every name double-quoted. Their provenance row in `tests/fixtures/README.md` names the Supabase row-level-security guide and the package's item names.
- AC3: guard.spec G8, one refusal test per fault naming it: an `http:` url to another host, a missing, empty and non-text key, a missing table, and the five listed table forms `Responses`, `1abc`, `a-b`, the empty string and a 64-character name (the 63-character name, `a`, `r2_d2` and `_x` accepted). `TABLE_NAME` is `/^[a-z_][a-z0-9_]{0,62}$/` (form.js line 104). link.spec L6: the builder refuses the url, key and table faults before building, one test each.
- AC4: the four T9 walks each assert no download event and the sent screen. send.spec T10: a table answering 401 and a refused connection each save the CSV (download event awaited, its header and lead fields read back) and show the failure screen naming the reason.
- AC5: README.md has "Send responses to Supabase" (line 216) covering the project, the SQL Editor paste, the key, the one-week pause and the Table Editor export. Hand run (work log, 2026-09-23): two HiTOP-BR walks into a table made from the builder's SQL, one with the `sb_publishable_` key and one with the legacy anon JWT, each answered 201; the export equals the posted bodies in 100 of 100 fields; with each key a select counted 0 rows and an update and a delete changed none. Committed as `tests/fixtures/supabase-hitopbr.csv` with its provenance row; send.spec T11 reads it.
- AC6 (first half): the checkout suite above; PR #3's Tests workflow on f06e1d6 passed (2 min 21 s); `git diff --stat main...HEAD -- . ':!cairn/'` in hitop is empty. The dispatched deployed-page run is recorded after the merge.
- AC6 (second half, 2026-09-23): PR #3's Tests run on the fixed head 034bd84 passed (2 min 38 s); PR #3 squash-merged as hitop-form c15e149; the push-triggered Tests run on c15e149 passed; the Pages deploy of c15e149 succeeded and the deployed link.html and form.js carry the new code; the dispatched run against the deployed page (run 35924564950) passed 111 of 111 with no skips, the local-network permission granted. hitop PR #123 merged as fa809196 with 8 of 8 checks green, cairn/ only.

Consistency gate 2026-09-23: `cairn_validate` exit 0 (advisories only); `devtools::document()` no diff; README.md newer than README.Rmd; `pkgdown::check_pkgdown()` no problems; NEWS.md needs no entry (the package changed nothing); `devtools::check()` 0 errors, 0 warnings, 0 notes (4 min 31 s). No principle changed, so no impact report.

Independent review, three lenses over hitop-form `main..HEAD` at f06e1d6. [S] blame-history: no findings (the webhook path's simple-request design, the nav-button disabling, the answers snapshot and the M111 tests are untouched). [S] prior-review record: one finding (F18). [O] diff-bug: 17 findings, none showing AC1–AC5 failing as written. Triage at the gate; Jeff chose to fix F7 too. Fix-now work landed in hitop-form commit after f06e1d6 (suite 111 of 111, was 100):
- F1 (fixed): a secret (`sb_secret_`) or a JWT-shaped key whose `role` is not `anon` was accepted; now refused by name in `checkStore()` (`jwtRole()`), with guard and builder tests.
- F2 (fixed): a redirect was followed and its 200 confirmed a Supabase send that stored nothing; now `redirect: 'manual'` on the supabase kind, an opaque redirect unconfirmed as "the endpoint redirected the send"; T10 gains a 302 case and asserts the twin saw nothing.
- F3 (fixed): a project URL with a path, query or fragment was accepted; now only the origin (after dropping `/rest/v1`) passes, four guard probes.
- F4 (noted): AC1 reads `<url>/rest/v1/<table>` with trailing slashes removed; the guard first drops a pasted `/rest/v1` suffix. The three walks AC1 names behave as written; the drop is a guard normalization outside AC1's domain, recorded under Decisions.
- F5 (fixed): the builder's button stayed enabled during the export fetch; now disabled until the build settles.
- F6 (fixed): T9 checked the four headers one by one; now also asserts the page's own header set equals exactly those names.
- F7 (fixed at Jeff's choice): the SQL now revokes the default grants from `anon` and `authenticated` before granting insert; fixtures and README updated; the provenance row says the hand-run table predates the line.
- F8 (fixed): the provenance row now cites the Postgres GRANT and REVOKE references for those lines.
- F9 (fixed): a key with a space or a non-ASCII character is refused by name.
- F10 (fixed): the recorder's insert route is anchored at `^/rest/v1/` and answers 405 to any method but POST.
- F11 (fixed): T11 compares the build date with the literal `2026-09-20`.
- F12 (fixed): L8 builds, then refuses on the same page, and asserts the link and SQL are cleared.
- F13 (fixed): form.js's header comment names the preflight.
- F14 (fixed): the link carries the project origin whatever suffix was pasted.
- F15 (fixed): the hint says "the last two".
- F16 (noted): AC5 names the network panel; the comparison used the body Playwright recorded leaving the browser, the same Chrome network data.
- F17 (rejected): item names over 63 bytes would break inserts; the package's names are 10 to 11 characters and no export can lengthen them without a package change.
- conversation: PR #3 — empty (no reviews, no comments, no unresolved threads at the gate).
- F18 (fixed, prior-review lens): the Supabase export opened in a spreadsheet could read `=1+1` as a formula, the gap M111's F2 closed for the sheet; the README now says the table stores text and points to the text-only R read.
