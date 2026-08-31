# M076: The builder page's boot path is bounded end to end and watched by a headless smoke test in CI

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m076-builder-smoke-ci` (both repos)

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
- [ ] T4. Add `.github/workflows/smoke.yml`: `pull_request` and `push` to
      `main` serving the checkout on localhost, `schedule` (weekly) and
      `workflow_dispatch` against the deployed page, with one retry before the
      job goes red.
- [ ] T5. Update the builder `README.md` (the runtime timeout beside the
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

## Decisions

## Review
