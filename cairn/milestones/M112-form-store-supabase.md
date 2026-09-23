# M112: hitop-form inserts each participant's responses into a Supabase table named in the study link

- **Status:** blocked
- **Priority:** normal
- **Depends on:** M111
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — a store kind researchers configure on the deployed page
- **Branch/PR:** hitop `m112-form-store-supabase` (cairn/ only); hitop-form `store-supabase`, PR #3 https://github.com/jmgirard/hitop-form/pull/3

## Goal

Add a second store kind to `jmgirard/hitop-form`, `supabase`, that inserts the finished responses as one row of a table the researcher owns, with the link builder writing the SQL that creates that table for the chosen form.

## Scope

**In:** the `supabase` kind on M111's send path (headers, confirmation on status, the same JSON row). The builder's three store fields (project URL, key, table name) and the SQL it shows: table, row-level security, an insert-only policy for `anon`. README setup and export steps, including the free-tier pause. One hand run against a real project, its export committed as a fixture. Participant-facing text stays outside instrument content (IP1); the SQL fixture is hand-written, never captured from the builder (IP2).

**Out:** Firestore → the online-form candidate row. Reading the export in the package → M113. A relay holding a REDCap token → ruled out by `cairn/references/online-collection.md` (a static page cannot hold one).

## Acceptance criteria

- [ ] AC1: A study link whose `store` is `{ "kind": "supabase", "url": <project URL>, "key": <key>, "table": <name> }` makes the page send one POST to `<url>/rest/v1/<table>`, with trailing slashes removed from `<url>`, when Finish is pressed. The request headers are `apikey: <key>`, `Content-Type: application/json`, `Prefer: return=minimal`, and `Authorization: Bearer <key>` only when the key has the three dot-separated segments of a JWT. The body is the JSON object M111's AC1 specifies. A send is confirmed on a 2xx status alone. Tests against M111's recording endpoint: one HiTOP-BR walk with a JWT-shaped key, one with an `sb_publishable_` key, and one with a project URL ending in a slash. They compare the URL, the headers with these literals, and the body with the fixture as M111's AC1 does, and assert that the endpoint recorded and answered one OPTIONS request per walk.
- [ ] AC2: For the chosen instrument or pasted module, link.html shows SQL that creates the table named in its field, the name double-quoted. The table has `study`, `participant`, `instrument`, `form_build` and `submitted` as `text`, then one `integer` column per item in the order the page will show them. The SQL enables row-level security, grants insert on the table to `anon`, and adds one policy `for insert to anon with check (true)`. A test compares the shown SQL for the HiTOP-BR and for the shuffled module fixture with hand-written fixtures under `tests/fixtures/`, whose provenance row names the Supabase policy documentation page and the package's item names.
- [ ] AC3: `parseLink()` refuses a `supabase` store whose `url` fails M111's AC4 rule, whose `key` is missing, empty or not a string, or whose `table` does not match `^[a-z_][a-z0-9_]{0,62}$`. Each refusal names the fault. link.html refuses the same three faults before building a link. Table forms probed: `Responses`, `1abc`, `a-b`, the empty string and a 64-character name. One guard test per listed form and one builder test per fault.
- [ ] AC4: The confirmed walks of AC1 fire no download event and show the sent screen. A walk against an endpoint answering 401 and a walk against a refused connection each save the CSV to the device and show the failure screen (two tests).
- [ ] AC5: The README gains a "Send responses to Supabase" section: creating a project, pasting the builder's SQL into the SQL editor, finding the key, the free project's pause after one idle week, and the Table Editor's CSV export. One hand run against a project whose table came from that SQL: one HiTOP-BR walk with the project's `sb_publishable_` key inserts one row, and the CSV export equals, field for field, the body the browser's network panel shows was posted. If the project still issues a legacy anon JWT, a second walk with it inserts a second row. With the same key, a REST select returns no rows, and an update and a delete change no rows. The export is committed as `tests/fixtures/supabase-hitopbr.csv` with a provenance row.
- [ ] AC6: `npx playwright test` passes against the checkout. The hitop-form pull request's Tests workflow is green, and a dispatched run against the deployed page passes after the merge under M111's AC7 local-network rule. The hitop repository's change is under `cairn/` only.

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
- [ ] T5: The README section. Create the project, run the walk, compare the export with the posted body, run the select, update and delete probes, commit the fixture and its provenance row. Work-log line: the date, the project region, and the probe results.
- [ ] T6: Open the hitop-form pull request at implement and merge it at review. Dispatch a run against the deployed page after the merge. In hitop, commit `cairn/` only.

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

## Decisions

## Review
