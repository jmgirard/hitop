# M112: hitop-form inserts each participant's responses into a Supabase table named in the study link

- **Status:** planned
- **Priority:** normal
- **Depends on:** M111
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — a store kind researchers configure on the deployed page
- **Branch/PR:** —

## Goal

Add a second store kind to `jmgirard/hitop-form`, `supabase`, that inserts the finished responses as one row of a table the researcher owns, with the link builder writing the SQL that creates that table for the chosen form.

## Scope

**In:** the `supabase` kind on M111's send path (headers, confirmation on status, the same JSON row). The builder's three store fields (project URL, key, table name) and the SQL it shows: table, row-level security, an insert-only policy for `anon`. README setup and export steps, including the free-tier pause. One hand run against a real project, its export committed as a fixture. Participant-facing text stays outside instrument content (IP1); the SQL fixture is hand-written, never captured from the builder (IP2).

**Out:** Firestore → the online-form candidate row. Reading the export in the package → M113. A relay holding a REDCap token → ruled out by `cairn/references/online-collection.md` (a static page cannot hold one).

## Acceptance criteria

- [ ] AC1: A study link whose `store` is `{ "kind": "supabase", "url": <project URL>, "key": <key>, "table": <name> }` makes the page send one POST to `<url>/rest/v1/<table>` when Finish is pressed. The request headers are `apikey: <key>`, `Content-Type: application/json`, `Prefer: return=minimal`, and `Authorization: Bearer <key>` only when the key has the three dot-separated segments of a JWT. The body is the JSON object M111's AC1 specifies. A send is confirmed on a 2xx status alone. Tests against M111's recording endpoint: one HiTOP-BR walk with a JWT-shaped key and one with an `sb_publishable_` key. They compare the URL, the headers with these literals, and the body with the fixture as M111's AC1 does, and assert that the endpoint recorded and answered one OPTIONS request per walk.
- [ ] AC2: For the chosen instrument or pasted module, link.html shows SQL that creates the table named in its field, the name double-quoted. The table has `study`, `participant`, `instrument`, `form_build` and `submitted` as `text`, then one `integer` column per item in the order the page will show them. The SQL enables row-level security, grants insert on the table to `anon`, and adds one policy `for insert to anon with check (true)`. A test compares the shown SQL for the HiTOP-BR and for the shuffled module fixture with hand-written fixtures under `tests/fixtures/`, whose provenance row names the Supabase policy documentation page and the package's item names.
- [ ] AC3: `parseLink()` refuses a `supabase` store whose `url` fails M111's AC4 rule, whose `key` is missing, empty or not a string, or whose `table` does not match `^[a-z_][a-z0-9_]{0,62}$`. Each refusal names the fault. link.html refuses the same three faults before building a link. One guard test per listed form and one builder test per fault.
- [ ] AC4: M111's AC2 outcomes hold for this kind: a walk against an endpoint answering 401 and a walk against a refused connection each save the CSV to the device and show the failure screen (two tests).
- [ ] AC5: The README gains a "Send responses to Supabase" section: creating a project, pasting the builder's SQL into the SQL editor, finding the key, the free project's pause after one idle week, and the Table Editor's CSV export. One hand run against a project whose table came from that SQL: one HiTOP-BR walk inserts one row, and the CSV export equals, field for field, the body the browser's network panel shows was posted. With the same key, a REST select returns no rows, and an update and a delete change no rows. The export is committed as `tests/fixtures/supabase-hitopbr.csv` with a provenance row.
- [ ] AC6: `npx playwright test` passes against the checkout. The hitop-form pull request's Tests workflow is green, and a dispatched run against the deployed page passes after the merge. The hitop repository's change is under `cairn/` only.

## Coverage

- AC1 → T3
- AC2 → T2
- AC3 → T1
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [ ] T1: Extend `checkStore()` with the `supabase` fields; guard and builder tests per AC3.
- [ ] T2: `storeSql()` in link.html for the chosen instrument or module, the hand-written SQL fixtures with their provenance row, and the comparison test.
- [ ] T3: The `supabase` branch of `sendResponses()`: URL, headers, the JWT test for `Authorization`, confirmation on status. Send tests with both key shapes, the OPTIONS assertion, and the with-store network walk.
- [ ] T4: The 401 and refused-connection tests.
- [ ] T5: The README section. Create the project, run the walk, compare the export with the posted body, run the select, update and delete probes, commit the fixture and its provenance row. Work-log line: the date, the project region, and the probe results.
- [ ] T6: Open the hitop-form pull request at implement and merge it at review. Dispatch a run against the deployed page after the merge. In hitop, commit `cairn/` only.

## Work log

- 2026-09-23: created by /milestone-plan. Promoted from the online-form candidate row (lineage M093); planned with M111 and M113.
- 2026-09-23: criteria audit ran in full mode on a fresh [O] reader (shared with M111): findings on this file were the `Authorization` header with a non-JWT key, the SQL fixture captured from the builder (IP2), the unverifiable "nothing else" in the policy claim, reserved-word table names, and the header-versus-fixture comparison. All fixed in the wording; none posed as a question.
- 2026-09-23: plan chose one `integer` column per item over a single `jsonb` column because the Table Editor's CSV export is then one tidy row per participant; falsified by a form whose column count exceeds Postgres's limit of 1,600.
- 2026-09-23: plan chose builder-generated SQL over a README template because a HiTOP-SR table has 410 columns nobody should type; falsified by a researcher needing a table shape the builder cannot express.

## Decisions

## Review
