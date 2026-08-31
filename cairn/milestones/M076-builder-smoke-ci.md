# M076: The builder page's boot path is bounded end to end and watched by a headless smoke test in CI

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m076-builder-smoke-ci` — https://github.com/jmgirard/hitop/pull/82 and https://github.com/jmgirard/hitop-builder/pull/12

## Goal

CI drives the browser module builder through a real headless boot and Word
download, and the page stops waiting forever on a webR runtime that never
arrives.

## Scope

Surface tier: **user-facing** — the workflow and smoke test are internal
tooling, but the runtime timeout changes shipped page behavior, so the
milestone spans both and is planned at the stricter tier. Two repos, as M064
was: `jmgirard/hitop-builder` takes the harness, the workflow and the page
change; this repo takes one `cairn/PROFILE.md` edit.

**In:** a committed Playwright smoke test driving the page to a Word download;
a plant matrix proving the test fails on defects it must catch; a workflow
running it on pull requests and on a weekly schedule against the deployed
page; a timeout around `import(WEBR_URL)` and `webR.init()`; the README and
PROFILE prose those need.

**Out:** the Qualtrics and REDCap download paths and the descriptor handover
button, and a distinct non-failing outcome when `webr.r-wasm.org` or
`jmgirard.r-universe.dev` is unreachable → both to the ROADMAP candidate row
added by this plan (lineage M076); the builder's own theme control, picker
copy and remaining comment-accuracy gaps → their standing candidate rows.

## Acceptance criteria

- [ ] AC1. A committed smoke-test script drives a headless browser through the
      page and asserts, in one run against a locally served copy of the
      repository's own `index.html` and one run against the deployed page, in
      this order: the status region reaches `Ready.`; more than 50 scale rows
      are present in `#scales` in the page's initial unfiltered state, each
      carrying a non-empty `.nm` name; and, after ticking one scale and
      clicking `#downloadBtn`, a file arrives whose first four bytes are
      `50 4B 03 04` and whose length exceeds 10,000 bytes. The 50 is written as
      a named constant whose comment states it is a deliberate floor and not
      the instrument's scale count. Evidence: the recorded output of both runs,
      beside the constant and its comment.
- [ ] AC2. The smoke test fails against each of six planted copies of
      `index.html`, and passes against the unplanted copy: (a) `MIN_HITOP`
      raised to `99.0.0`, unreachable by construction; (b) `#downloadBtn`
      renamed; (c) `WEBR_URL` pointed at a path that returns 404; (d) the
      download handler made to hand over a 12-byte non-ZIP blob in place of
      the generated file; (e) the picker's render truncated to one scale row;
      (f) one rendered row given an empty `.nm` name. Evidence: the seven
      recorded runs, each named with the copy it ran against, and for each
      planted run the assertion that failed — so every assertion the smoke
      test makes, enumerated from the spec file, is failed by at least one of
      the six.
- [ ] AC3. A workflow in `jmgirard/hitop-builder` runs the smoke test on
      `pull_request` and `push` to `main` against the repository's own
      `index.html`, and on a weekly `schedule` and on `workflow_dispatch`
      against the deployed page. Evidence: one green `pull_request` run and
      one green `workflow_dispatch` run recorded by URL; `push` and `schedule`
      verified by quoting the `on:` block, `schedule` because GitHub fires it
      only from the default branch and no pre-merge run of it exists.
- [ ] AC4. `index.html` races the webR runtime download — `import(WEBR_URL)`
      and `webR.init()` together, under one timeout constant declared beside
      `INSTALL_TIMEOUT_MS` — and on expiry calls `abandonBoot()` with a
      message naming the runtime download rather than the package install.
      Evidence: two probes, one serving a copy whose `WEBR_URL` never settles
      and one whose `WEBR_BASE` never settles, each recording the displayed
      message and that `#controls` is hidden.
- [ ] AC5. The builder `README.md` states the runtime timeout beside the
      install timeout it already states, in a number equal to the constant in
      `index.html`; its "Every tracked file" table lists every file this
      milestone adds to that repo; and this repo's `cairn/PROFILE.md`
      Downstream step names the smoke test and the four triggers AC3 builds.
      Evidence: the three passages quoted, beside the constant they mirror.

## Coverage

- AC1 → T1
- AC2 → T1, T2
- AC3 → T4
- AC4 → T3
- AC5 → T5

## Tasks

- [x] T1. Add the harness to `hitop-builder`: `package.json` plus a lockfile
      pinning `@playwright/test`, and a smoke spec taking its target from an
      env var and defaulting to a locally served checkout. Budget the run for
      a ~20s cold webR load (per-assertion waits well past Playwright's 30s
      default); capture the download through Playwright's `download` event —
      the page hands the file over on an anchor with a `download` attribute,
      and webR's own requests come from a Web Worker (the M045 lesson).
- [x] T2. Add the plant matrix: a committed script writing the six planted
      copies of `index.html` (AC2 a–f) to a scratch directory, running the
      smoke test against each and against the unplanted copy, and printing
      which assertion failed for each, against the spec file's own
      enumerated assertions.
- [x] T3. In `index.html`, declare `RUNTIME_TIMEOUT_MS` beside
      `INSTALL_TIMEOUT_MS` (`index.html:640`) and race the `import(WEBR_URL)`
      + `webR.init()` pair against it inside `main()` (`index.html:1277-1291`),
      calling `abandonBoot()` on expiry with a message naming the runtime
      download. Follow the existing install race: handle the loser's rejection
      in place so a late settle cannot reach an unhandled rejection or turn a
      control back on past the `bootAbandoned` latch.
- [x] T4. Add `.github/workflows/smoke.yml`: `pull_request` and `push` to
      `main` serving the checkout on localhost, `schedule` (weekly) and
      `workflow_dispatch` against the deployed page, with one retry before the
      job goes red.
- [x] T5. Update the builder `README.md` (the runtime timeout beside the
      install timeout in "How it works"; the repository-layout table) and this
      repo's `cairn/PROFILE.md` Downstream step (`PROFILE.md:81-90`).

## Work log

- 2026-08-31: created by /milestone-plan. Absorbs the ROADMAP candidate row
  for builder CI (lineage M045, M064); M064's archive records this gap.
- 2026-08-31: plan gate criteria audit ran in FULL mode (declared surface tier
  user-facing). It returned five findings. Three were fixed here and reported:
  AC3's evidence list now says which triggers are quotation-verified and why,
  AC4 became one timeout over both steps with a probe for each, and AC5's
  PROFILE clause now describes the trigger set AC3 actually builds rather than
  a release-walk hook nothing here creates. Two went to the gate as questions
  (AC1's scale floor, AC2's probe family). AC1 and AC2 were re-audited after
  the gate changed them.
- 2026-08-31: AC1 and AC2 were re-audited after the gate changed them, in the
  same full mode. Four further findings, all fixed here: AC2's closing clause
  was false of its own plant set (no plant failed the non-empty-name
  assertion), so plant (f) was added; the clause quantified over AC1's prose
  where plant (b) already fails an assertion AC1 does not state, so it now
  quantifies over the spec file's enumerated assertions; AC1's floor must
  carry a comment fixing it as a floor, so a later reader does not "tighten"
  it to 76 and import a scale-membership fact into the builder repo; and AC1
  now orders `Ready.` before the click (it is also the post-build status) and
  plant (a) pins `99.0.0` rather than a version that decays into reachable.
  The audit confirmed `> 50` is not IP1 content: IP1's domain is keying
  tables, scale memberships, item text, response options and instructions,
  and a floor of 50 needs no sign-off against a source to stay true.
- 2026-08-31: plan gate chose Playwright over Puppeteer and over a
  network-free static check of `index.html`; Puppeteer offers no download
  capture worth the difference and a static check cannot catch r-universe
  serving a `hitop` the page no longer matches, which is the break the row was
  written about; falsified by Playwright failing to drive webR's Web Worker
  requests headlessly.
- 2026-08-31: plan gate chose a local-checkout job on pull requests plus a
  deployed-page job on a schedule, over testing the deployed page only; a
  deployed-only test cannot fail on the change under review; falsified by the
  local and deployed runs disagreeing for reasons other than the diff.
- 2026-08-31: plan gate chose one retry then a red X over probing
  `webr.r-wasm.org` and `jmgirard.r-universe.dev` for reachability first; a
  reachability probe is a second thing that can be wrong and can mask a real
  break that looks like an outage; falsified by false reds becoming routine
  enough to be ignored.
- 2026-08-31: plan gate chose the Word download path alone over all three
  formats plus the descriptor handover, and a `>50` scale-row floor over
  pinning 76; a per-format run triples the weekly job's ways to go red on one
  upstream hiccup, and an exact count writes instrument content into a repo
  that holds none; falsified by a generator breaking in Qualtrics or REDCap
  alone, or by a scale-list regression landing between 2 and 50 rows.
- 2026-08-31: implement gate settled three choices: the runtime timeout is two
  minutes, the same as the install timeout; the Playwright dependency is
  recorded as a cross-cutting D-entry; the timeout message names R itself
  rather than the host it comes from.
- 2026-08-31: tasks reordered to T3, T1, T2, T4, T5 so the smoke test and the
  plant matrix run against the page as this milestone leaves it. Plan wording
  unchanged.
- 2026-08-31: T1 done in jmgirard/hitop-builder (`1b5ec6f`): `package.json`,
  `package-lock.json` pinning `@playwright/test` 1.56.1, `playwright.config.js`,
  `tests/serve.mjs` and `tests/smoke.spec.js`. Runs green against the local
  checkout and against the deployed page.
- 2026-08-31: T3 done in jmgirard/hitop-builder (`63584dd`): `RUNTIME_TIMEOUT_MS`
  = 120000 beside `INSTALL_TIMEOUT_MS`, racing `import(WEBR_URL)` and
  `webR.init()` together. `tests/runtime-timeout.spec.js` stalls each half
  against a request the local server never answers; both see the message and
  `#controls` hidden, where the page before this change sits on "Starting R in
  your browser…" indefinitely.
- 2026-08-31: T2 done in jmgirard/hitop-builder (`ea14c2f`). Matrix run: the
  unplanted copy passed; (a), (b) and (c) failed A1, (d) failed A4 and A5, (e)
  failed A2, (f) failed A3 — all five enumerated assertions covered, the list
  read from the spec file rather than restated.
- 2026-08-31: T4 done in jmgirard/hitop-builder (`ab87b59`):
  `.github/workflows/smoke.yml`, four triggers, `SMOKE_TARGET` set to the
  deployed page on `schedule` and `workflow_dispatch` and empty otherwise.
  Retries come from `playwright.config.js`, which allows one under `CI`.
- 2026-08-31: T5 done. Builder `README.md` (`82cff5a`) states both timeouts as
  `120000` milliseconds and lists the seven files this milestone adds; this
  repo's `cairn/PROFILE.md` gained a Downstream watch bullet naming the smoke
  test and its four triggers (committed early, in T1's tracking commit
  `295b6508`, and trimmed to keep the file under its 120-line cap).
- 2026-08-31: D-051 recorded — the builder repo's `@playwright/test`
  development dependency, per the implement gate.
- 2026-08-31: AC3 runs green on the branch — `pull_request`
  https://github.com/jmgirard/hitop-builder/actions/runs/33426624284 (this
  checkout) and `workflow_dispatch`
  https://github.com/jmgirard/hitop-builder/actions/runs/33426646583 (the
  deployed page). Dispatching from a non-default branch worked, so no
  amendment to AC3's evidence list was needed.
- 2026-08-31: all tasks done; `devtools::test()` clean (0 failures, 16228
  passes, 4 skips) with no R source touched. Status set to review.

- 2026-08-31: /milestone-review returned M076 to in-progress. AC1 failed: the
  smoke test's A3 asserts that no collected `.nm` name is blank, not that every
  scale row carries one, so its locator can silently empty and the run stay
  green — AC1 requires "each carrying a non-empty `.nm` name". AC2-AC5 verified
  with fresh evidence and ticked. Eight further findings (F2-F9) go back with
  it; two (F10, F11) become candidate rows; one rejected. Details in the Review
  section.
- 2026-08-31: implement gate settled the one open choice in the review return —
  the Pages deploy stages `index.html` alone rather than uploading the whole
  checkout, chosen over correcting the three places that already said the
  deployed site is that one file. Keeps `README.md`, the `package.json`
  description and D-051 true as written, and stops serving the test files at
  the public URL.
- 2026-08-31: F1 (the return) fixed in jmgirard/hitop-builder (`d9a7819`). A3
  now asserts one non-empty `.nm` name per counted row, not merely that no
  collected name is blank. Check-discrimination probe: a scratch copy with
  `name.className` and the filter's selector renamed `nm` → `nmx` fails A3
  (expected `"named": 76`, received `"named": 0`), which the previous
  assertion passed over.
- 2026-08-31: F2-F8 fixed in the same commit. F2: every run logs and annotates
  the URL it drove, and `smoke.yml` sets `SMOKE_REQUIRE_TARGET` on `schedule`
  and `workflow_dispatch` so an empty `SMOKE_TARGET` fails instead of falling
  back (probe: the guard fires). F3: per-test budget 8 → 10 minutes over the
  480s of waits inside it, plus a 30s `actionTimeout`; two attempts still fit
  the 25-minute job. F4: plant (b) renames the button at all three sites, so
  the page boots and the plant fails A6 rather than A1 on a page-init crash.
  F5: a plant run with no usable report is an error the matrix fails on. F6:
  `pages.yml` stages `index.html` into `_site`. F7: the runtime race's
  `no import` and `failed` branches latch through `abandonBoot()`. F8: the
  `bootAbandoned` comment lists all five call sites.
- 2026-08-31: F9's premise did not hold. `plants.mjs` held a literal escape
  byte (`0x1b`) before `\[`, invisible in rendered text, so the regex already
  stripped ANSI; verified by `od -c`. Rewritten as `\x1b` for a readable
  source line, same match.
- 2026-08-31: added A6 to the spec's enumerated assertions (the download button
  is present and enabled), asserted before the click so plant (b) fails a named
  assertion rather than a bare locator timeout. AC1 and AC2 quantify over the
  spec file's enumerated list, so no criterion text changed.
- 2026-08-31: fresh runs after the fixes — local-checkout smoke green (9.6s),
  deployed-page smoke green (9.1s), plant matrix green overall with the
  unplanted copy passing and (a) A1, (b) A6, (c) A1, (d) A4+A5, (e) A2, (f) A3,
  both runtime probes green.
- 2026-08-31: AC2-AC5 unticked. Every one rests on evidence taken at branch
  head `82cff5a` — plant (b) failing A1, a `README.md` table row, two workflow
  runs, two runtime probes — and this pass moved the head and changed the
  artifacts under all four. No criterion text changed; all four re-verify at
  review.

## Decisions

## Review

Reviewed 2026-08-31 on `m076-builder-smoke-ci`. `origin/main` had not moved
since the branch was cut, so no merge was needed. PRs:
https://github.com/jmgirard/hitop/pull/82 and
https://github.com/jmgirard/hitop-builder/pull/12; CI green on both.

### Acceptance criteria

- AC1 — **not met**. Both runs passed: local server over this checkout (12.1s)
  and `SMOKE_TARGET=https://jmgirard.github.io/hitop-builder/` (9.9s). The
  ordering, the ZIP magic bytes, the 10,000-byte floor and the named constant
  are all as AC1 states — `const MIN_SCALE_ROWS = 50;` carries the comment "A
  deliberate floor, set well under the number of scales the instrument actually
  has. It is NOT the instrument's scale count and must not be 'tightened' into
  one". What the spec does not assert is AC1's "each carrying a non-empty `.nm`
  name". A3 (`smoke.spec.js:78-82`) reads `#scales label .nm` and asserts that
  no collected name is blank; with no `.nm` elements at all
  `allTextContents()` returns `[]` and the assertion passes over an empty
  domain. A2 floors `#scales label`, a different locator, so the in-file
  comment "A2 having floored the count above zero is what keeps this from
  passing over an empty list" is false of the locator A3 actually uses. A
  render that drops the name span or renames its class leaves 76 rows, no
  names, and a green smoke test.

- AC2 — met. `npm run plants` at review time, reading the five enumerated
  assertions out of `tests/smoke.spec.js` rather than restating them: unplanted
  copy passed; (a) `MIN_HITOP` = `99.0.0` failed A1; (b) `#downloadBtn` renamed
  failed A1; (c) `WEBR_URL` at a 404 path failed A1; (d) 12-byte non-ZIP blob
  failed A4 and A5; (e) render truncated to one row failed A2; (f) one row
  given an empty name failed A3. All five assertions failed by at least one
  plant. See finding F4 on what plant (b) actually proves.

- AC3 — met. `.github/workflows/smoke.yml` declares all four triggers: `on:
  pull_request:` / `push: branches: [main]` / `schedule: - cron: '0 6 * * 1'` /
  `workflow_dispatch:`. `SMOKE_TARGET` is set to
  `https://jmgirard.github.io/hitop-builder/` when `github.event_name` is
  `schedule` or `workflow_dispatch` and to the empty string otherwise, and the
  spec serves the checkout when it is empty. Green runs on the branch head
  `82cff5a`: `pull_request`
  https://github.com/jmgirard/hitop-builder/actions/runs/33426624284 (the
  checkout) and a review-time `workflow_dispatch`
  https://github.com/jmgirard/hitop-builder/actions/runs/33429940196 (the
  deployed page, 19.8s). `push` and `schedule` are verified by the quoted `on:`
  block, `schedule` because GitHub fires it only from the default branch.
- AC5 — met. Builder `README.md:52-56`: "is raced against
  `RUNTIME_TIMEOUT_MS`, and `installPackages` against `INSTALL_TIMEOUT_MS`.
  Both are stated in `index.html`, both set to `120000` milliseconds — two
  minutes" — equal to the two constants above. Its "Every tracked file" table
  lists all eight files the branch adds (`.github/workflows/smoke.yml`,
  `package.json`, `package-lock.json`, `playwright.config.js`, and the four
  files under `tests/`); `git diff --name-status origin/main..HEAD` shows no
  ninth. This repo's `cairn/PROFILE.md:90-95` Downstream watch bullet names
  `tests/smoke.spec.js` and `.github/workflows/smoke.yml` on the four triggers:
  "`pull_request` and `push` to `main` against its own checkout, a weekly
  `schedule` and `workflow_dispatch` against the deployed page".

### Consistency gate

Universal cairn-file checks: `cairn_validate.py` exits 0, all checks PASS.
Advisories are the standing ones (`work-log format`, `dangling id tokens`,
`references staleness`) and unchanged by this milestone; `release window` did
not fire. No principle changed (`Principles touched: —`), so `cairn_impact.py`
was skipped.

Toolchain checks from the `r-package` profile's `consistency-gate` slot:
`devtools::document()` produced no diff; `devtools::test()` 0 failures / 16228
passes / 4 skips; `devtools::check()` 0 errors, 0 warnings, 0 notes;
`pkgdown::check_pkgdown()` "No problems found"; `check_line_endings.R` passed.
No generated file was hand-edited and no R source was touched. NEWS.md needs no
entry: nothing in this milestone changes the package's own behavior.

### Independent review

Three fresh-context reviewers, none having seen the implementation, on distinct
evidence bases. The blame-history lens found one item; the prior-review lens
found none (`gh api .../pulls/comments` returned `[]` on both repos, and M064's
archived review records the untimed runtime download as the gap this milestone
closes). The diff-bug lens returned eleven, ranked below with disposition.

- F1 (return) — A3 passes over an empty domain. `tests/smoke.spec.js:78-82`
  asserts no collected `.nm` is blank, not that every row has one; the locator
  can silently empty independently of A2's floor, and the comment justifying it
  names the wrong locator. Fails AC1 as written. **Returns the milestone.**
- F2 (fix now) — a `schedule` or `workflow_dispatch` run can silently test the
  checkout instead of the deployed page: `smoke.spec.js:32-52` falls back to a
  local server whenever `SMOKE_TARGET` is empty and never records the URL it
  used, so a dropped `env:` block or a renamed event leaves the weekly job
  green forever against the wrong page.
- F3 (fix now) — `BOOT_MS + BUILD_MS` (240000 + 240000) equals
  `playwright.config.js`'s `timeout` of 480000 exactly, leaving no headroom for
  `goto`, the clicks or the checkbox; on a slow runner the test dies with a
  bare "Test timeout exceeded", A4/A5 are never evaluated, and `plants.mjs`
  records no assertion for that run. No `actionTimeout` is set either.
- F4 (fix now) — plant (b) proves something other than what it claims. The
  renamed `#downloadBtn` makes `main()` throw on `el('downloadBtn')` at
  `index.html:1497` before `status('Ready.')`, so the run fails A1 on a page-init
  crash and the download click is never reached. AC2's letter holds (A1 is
  covered), but the plant's stated defect goes untested.
- F5 (fix now) — `plants.mjs:106-125` resolves a run whose stdout is not JSON,
  or whose report has no results, to `{passed: false, failed: []}`, so a
  transient launch error on a plant that shares its assertion with another
  plant is credited as "the smoke test went red".
- F6 (fix now) — "the deployed site is `index.html` alone" is now false.
  `.github/workflows/pages.yml` uploads `path: .`, so `package.json`,
  `playwright.config.js` and all of `tests/` are served. Stated as fact in
  `README.md:355-357`, `package.json:5`, and D-051.
- F7 (fix now) — the runtime race's `'failed'` branch (`index.html:1329-1332`)
  does not call `abandonBoot()`, unlike its install counterpart at
  `:1396-1404`. Not reachable as a bug today; the asymmetry is.
- F8 (blame lens, fix now) — the `bootAbandoned` comment at
  `index.html:656-660` enumerates two causes ("the install timed out, or the
  installed package is older than `MIN_HITOP`"); this milestone adds a third
  and did not update it.
- F9 (fix now) — `plants.mjs:129` strips ANSI with `/\[[0-9;]*m/g`, omitting
  the escape byte, so a stray `\x1b` survives into the printed note.
- F10 (follow-up) — `plants.mjs:117-121` walks only top-level suites; adding a
  `test.describe(...)` to the spec would nest the specs and make every run,
  planted and unplanted, read as failed. → candidate row.
- F11 (follow-up) — `plants.mjs` and `runtime-timeout.spec.js` pin literal
  `MIN_HITOP`, `WEBR_URL`, `WEBR_BASE` and `RUNTIME_TIMEOUT_MS` strings from
  `index.html`; a version bump breaks them. `replaceOnce` makes each break
  loud, which is the right design, but nothing tells the person doing the bump.
  → candidate row.
- F12 (reject) — `serve.mjs:34-37` never removes aborted `/hang/` responses
  from its Set. Bounded by the run and harmless; a pure tidiness point.

Reviewer clearances worth recording: the four workflow triggers and the
`SMOKE_TARGET` expression evaluate as the comments and the prose claim; the
`concurrency` key genuinely stops a `push` cancelling a `schedule`; the A4/A5
soft assertions are used correctly and the `download` wait is a hard timeout;
the runtime race's loser cannot reach an unhandled rejection and cannot
re-enable a control past the `bootAbandoned` latch; and no recorded decision,
past guard or archived review finding is undone by this diff.

### Outcome

Returned to `in-progress` on F1: AC1 requires the smoke test to assert that
every scale row carries a non-empty `.nm` name, and it does not. F2-F9 go back
with it; F10 and F11 become candidate rows at the next review.

