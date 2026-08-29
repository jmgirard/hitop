# M064: The builder refuses loudly when the package it installs no longer matches it

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m064-builder-version-guard` (hitop), https://github.com/jmgirard/hitop/pull/71 · `m064-version-guard` (hitop-builder), https://github.com/jmgirard/hitop-builder/pull/10

## Goal

Stop the builder page breaking mid-build when r-universe serves a `hitop` it no
longer matches, and stop it waiting forever on an install that never finishes.

## Scope

Surface tier: **user-facing** — the deployed page's visitors are external
consumers of this deliverable.

**In:** a declared minimum `hitop` version in `jmgirard/hitop-builder`'s
`index.html`, compared against the installed version at boot; a timeout on the
`installPackages` call with a latch so a late-settling install cannot re-enable
the controls; that repo's `README.md`; and a release-walk step in this repo's
`cairn/PROFILE.md` so a `hitop` release updates the page's minimum.

**Out:** pinning the installed version — r-universe serves only its current
build and `webR.installPackages` takes no version, so a hard pin is not
available; the minimum-version refusal is what replaces it. An automated job that
loads the deployed page and builds a form stays a candidate row. The handover and
the filenames → M062, M063.

## Acceptance criteria

- [x] AC1 The page declares a minimum `hitop` version and compares it against the
      installed version with `numeric_version`, not string comparison; below the
      minimum the page stops with a message naming the declared minimum, the
      installed version and where to get a working page, and no download control
      is enabled. Probed with the installed version forced below, equal to and
      above the declared minimum, and on a pair whose components differ in digit
      count (`0.9.0` against `0.10.0`), so the comparison is exercised in both
      directions and in both version forms.
- [x] AC2 A stalled install does not hang and cannot un-stall itself: the
      `installPackages` call is raced against a stated timeout; on timeout the
      status line says the install did not finish and what to try, and the
      download controls stay disabled. Probed twice — a timeout that fires with
      the install still pending, and a timeout that fires before an install which
      then settles — with the controls asserted still disabled in both.
- [x] AC3 `cairn/PROFILE.md`'s release-walk slot carries a step naming
      `jmgirard/hitop-builder` and what a `hitop` release must update there.
- [x] AC4 `README.md` states the declared minimum in exactly one prose location
      and says `index.html` is where it is declared. The sample descriptor's
      `packageVersion` is excluded by name, since it records the version that
      wrote a file rather than a minimum.
- [x] AC5 The change ships: a merged pull request in `jmgirard/hitop-builder`
      whose URL is in this file's header, and the page served at
      `https://jmgirard.github.io/hitop-builder/` matches that commit's
      `index.html` byte for byte.
- [x] AC6 The `hitop` package's code, tests, data and documentation are
      untouched — `git diff --name-only` against the merge base lists nothing
      outside `cairn/` — and `devtools::test()` is clean.

## Coverage

- AC1 → T1, T4
- AC2 → T2, T4
- AC3 → T3
- AC4 → T5
- AC5 → T6
- AC6 → T1, T2, T5, T6

## Tasks

- [x] T1 Declare `MIN_HITOP` in `index.html` beside the other boot constants
      (`hitop-builder/index.html:546-549`), compare it against the version the
      page already reads (`:1079-1081`) via `utils::compareVersion` or an R-side
      `numeric_version` test, and take the refusal path.
- [x] T2 Race `webR.installPackages` (`index.html:1072`) against a stated timeout
      with a latch that keeps the controls disabled once the timeout has fired.
- [x] T3 Add the builder step to `cairn/PROFILE.md`'s release-walk slot.
- [x] T4 Drive the four version probes and the two timeout probes in the browser
      pane against a local copy of the page, with the compared values and the
      timeout forced from the page's own constants.
- [x] T5 Rewrite `README.md:36-37` for the declared minimum and leave the sample
      descriptor at `:114` alone.
- [x] T6 Open the builder pull request and write its URL into the header. The
      merge and the deployed-bytes comparison AC5 also asks for are the review
      gate's, not implement's — see the work log.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier); returned 3 findings on this milestone — a one-direction version probe blind to a lexical comparison getting `0.10.0` and `0.9.0` backwards, an unprobed hazard where an uncancellable install settles after the timeout and re-enables the controls, and an unsatisfiable "appears in one place" over a README carrying a version in two different roles — all fixed before the criteria were written.
- 2026-08-29: T1/T2 — `index.html` declares `MIN_HITOP = '0.2.0'` and `INSTALL_TIMEOUT_MS = 120000` beside the other boot constants; the install is raced against the timeout and the installed version is read before `library()` and compared by `installedIsOlderThan()`, which binds both versions into R as data and tests `numeric_version(a) < numeric_version(b)`. Either failure calls a new `abandonBoot()`, which sets a `bootAbandoned` latch, hides the controls and disables the download and handover buttons; the latch is also read at the three sites that turn a control back on, so an install settling after the timeout cannot re-enable one. T1 refined against AC1: `numeric_version`, not the `utils::compareVersion` the task offered as an alternative, since AC1 names `numeric_version` and criteria outrank task wording.
- 2026-08-29: all tasks done, status review. `devtools::test()` clean (FAIL 0, WARN 0, SKIP 4, PASS 15504); the hitop branch diff against `origin/main` is `cairn/PROFILE.md`, `cairn/ROADMAP.md` and this file, nothing outside `cairn/`.
- 2026-08-29: T6 (minor amendment, task wording only — no criterion changed) — the builder pull request is https://github.com/jmgirard/hitop-builder/pull/10 and its URL is in the header. T6 as planned also carried the merge and the deployed-bytes comparison; both are held for `/milestone-review`, because nothing reaches a default branch without the user's approval at the merge gate, and because AC5 is a criterion review verifies with fresh evidence rather than one implement can tick for itself. M063 merged its builder pull request the same way, at review.
- 2026-08-29: T4 — six probes driven in the browser pane against `http://localhost:8788` serving copies of `index.html` with one constant altered each, deleted afterwards. Version: the real `installedIsOlderThan()` returned `TRUE` for 0.1.0<0.2.0 and 0.9.0<0.10.0, `FALSE` for 0.2.0<0.2.0, 0.3.0<0.2.0 and 0.10.0<0.9.0 — the last two pairs are the ones a string comparison gets backwards, so the check is shown able to fail the way it claims to catch. End to end: installed 0.2.0 against a declared 0.2.0 and 0.1.0 both reached "Ready." with the controls shown and 76 scale rows rendered; against a declared 99.0.0 the page stopped with a status naming 99.0.0, 0.2.0 and jmgirard.github.io/hitop, `#controls` hidden and `downloadBtn`/`saveDescriptor` both disabled. Timeout: with `INSTALL_TIMEOUT_MS = 1` the timeout fired at 1992 ms with the install still pending and the page stopped with both buttons disabled; the same install then settled at 9537 ms, 7.5 s after the timeout, with `bootAbandoned` already true and both buttons disabled at that instant, and both still disabled 30 s later.
- 2026-08-29: T5 — `README.md`'s "How it works" paragraph rewritten: it states the declared minimum (0.2.0) in one prose location, names `index.html` as where `MIN_HITOP` is set, and explains the component-wise comparison and the refusal; a second paragraph states the install timeout and its constant. A sentence under the sample descriptor says its `packageVersion` is the version that wrote that file, not the minimum. The other two `version` mentions in the file are a webR link and a scale-definitions aside, neither a version number.
- 2026-08-29: T3 — `cairn/PROFILE.md`'s release-walk slot gained a "Downstream" step naming `jmgirard/hitop-builder`, the six package surfaces the page calls, and the two things a release touching one of them must update there (`MIN_HITOP` in `index.html`, the minimum stated in that repo's `README.md`). File is 110 lines against the 120-line cap.
- 2026-08-29: gate chose `MIN_HITOP = '0.2.0'` (the current released version, and the one every page-called surface is present in) and a 120-second install timeout (six times the README's stated twenty-second first load).
- 2026-08-29: /milestone-review — hitop PR #71 (draft) opened for the tracking half; AC1-AC4 and AC6 verified with fresh evidence, AC5 held for the merge gate; consistency gate clean (`cairn_validate` exit 0, `document()` no diff, `check_pkgdown()` clean, `devtools::check()` 0 errors 0 warnings 0 notes); three-lens review returned nine findings, none floor-qualifying — five to the maintainer at the gate, one follow-up candidate, three rejected.
- 2026-08-29: gate — maintainer chose to fix four of the five findings before merging (finding 6's wording left as is) and approved merging both pull requests; hitop-builder `3d91721` lands the fixes and every branch was re-probed clean, including a discriminating channel-loss probe.
- 2026-08-29: builder pull request squash-merged as `2a7f2ae`; Pages redeployed and the served page is byte-identical to that commit's `index.html` (64,734 bytes, matching SHA-256) — AC5 verified, every criterion now ticked against recorded evidence.
- 2026-08-28: plan chose a declared minimum plus a release-walk step over a release-walk step alone (rejected: it keeps the page correct only for as long as nobody forgets, and the failure it misses is silent mid-build) and over a hard version pin (rejected as unavailable — r-universe serves only its current build and the install call takes no version); falsified by r-universe gaining versioned installs, which would make a pin the better answer.

## Decisions

## Review

### Acceptance criteria

- **AC1 — verified 2026-08-29.** Probed fresh in the browser pane against
  `http://localhost:8788` serving copies of the branch's `index.html` with one
  constant altered each, deleted afterwards. `installedIsOlderThan()` called
  directly on seven pairs returned `TRUE` for 0.1.0<0.2.0, 0.9.0<0.10.0 and
  0.2.0<0.10.0, `FALSE` for 0.2.0<0.2.0, 0.3.0<0.2.0, 0.10.0<0.9.0 and
  0.10.0<0.2.0 — the two 0.10.0/0.9.0 pairs are the ones a string comparison
  reverses, so the comparison is exercised in both directions and in both digit
  forms, and the check is shown able to fail the way it claims to catch. End to
  end with the installed 0.2.0: declared 0.2.0 (equal) and 0.1.0 (below) both
  reached "Ready." with `#controls` shown and 78 scale checkboxes rendered;
  declared 99.0.0 stopped the page with the status "This page needs hitop
  99.0.0 or newer, and the version it just installed is 0.2.0. … A working page
  and the complete instruments are at jmgirard.github.io/hitop.", `#controls`
  hidden, `downloadBtn` and `saveDescriptor` both disabled and zero enabled
  buttons under `.downloads`/`.handover`.

- **AC2 — verified 2026-08-29.** Same server, a copy with
  `INSTALL_TIMEOUT_MS = 1`. Probe one: the timeout fired with the install still
  pending and the page stopped with "The hitop package did not finish
  downloading within 0.001 seconds, so this page cannot build anything. Reload
  to try again; if it keeps failing, the complete instruments are ready to
  download at jmgirard.github.io/hitop." — `#controls` hidden, `downloadBtn`
  and `saveDescriptor` disabled, zero enabled download or handover buttons.
  Probe two: with the page already latched off, `packageVersion("hitop")`
  polled through webR first succeeded 3,705 ms later, returning 0.2.0 — the
  abandoned install had settled — and at that instant `bootAbandoned` was true
  with every control still disabled and `#controls` still hidden; re-checked 25
  seconds after that, all still disabled.

- **AC3 — verified 2026-08-29.** `cairn/PROFILE.md`'s `## release-walk` slot
  ends with a "Downstream" bullet naming `jmgirard/hitop-builder`, the six
  package surfaces the page calls (`available_scales()`, `hitop_module()`,
  `scale_definitions()`, the three `generate_*_hitopsr()` functions and their
  `descriptor` argument), and the two things a release touching one of them
  must update there — `MIN_HITOP` in `index.html` and the minimum stated in
  that repo's `README.md`, in a pull request of its own. File is 110 lines
  against the profile's 120-line cap.

- **AC4 — verified 2026-08-29.** `grep -n '0\.2\.0' README.md` in
  `hitop-builder` returns two lines: `:42`, the prose sentence "today it is
  **0.2.0**", and `:141`, the `packageVersion` field inside the sample
  descriptor's JSON block — so the minimum appears in exactly one prose
  location. `:41` says `MIN_HITOP` in `index.html` "is the one place that
  minimum is set", and `:151-152` says the sample's `packageVersion` "is the
  version that wrote this particular file, not the minimum", naming the
  exclusion.

- **AC6 — verified 2026-08-29.** `git diff --name-only origin/main...HEAD`
  lists `cairn/PROFILE.md`, `cairn/ROADMAP.md` and this file — nothing outside
  `cairn/`. `devtools::test()` clean: FAIL 0, WARN 0, SKIP 4, PASS 15504.

- **AC5 — verified 2026-08-29.** jmgirard/hitop-builder#10 squash-merged as
  `2a7f2ae` on that repo's `main`, its URL in this file's header; the Deploy to
  Pages run for that commit finished successfully. `git show
  2a7f2ae:index.html` and the page fetched from
  `https://jmgirard.github.io/hitop-builder/` are both 64,734 bytes with SHA-256
  `f191d835f71c9cfa8ebb2da9bb680bae48a53934df84147115f1c85f72b7635f`, and `cmp`
  reports no difference.

### Consistency gate

- `cairn_validate.py` exit 0 — all 16 checks PASS, 4 OK, 21 advisory warnings
  (20 dangling `D-00N` id tokens in DESIGN/DECISIONS/SOURCES and one
  references-staleness line on `schmukle2026.md`, all pre-existing and none a
  gate failure). `coverage complete` and `scaffold present` both PASS.
- No `DESIGN.md` principle changed on this branch, so `cairn_impact.py` was
  skipped.
- r-package profile `consistency-gate`: `devtools::document()` produced no diff;
  `pkgdown::check_pkgdown()` "No problems found."; `README.md` is current
  against `README.Rmd`; no new top-level files, so no `.Rbuildignore` entry is
  owed; no user-visible package change, so no NEWS entry is owed;
  `devtools::check()` 0 errors, 0 warnings, 0 notes.

### Independent review — three lenses, fresh context

Full three-lens fan-out (user-facing tier, executable surface touched). Every
reported finding is listed with its disposition.

**[O] diff-bug (Opus), ranked:**

1. The "Lost the connection to R" handler disables `.downloads button` directly
   and returns without setting `bootAbandoned`, so if the R channel dies after
   `#controls` is shown, the next `refreshTally()` (any checkbox) re-enables the
   build button over a dead R. — Confirmed against the implementation
   (`index.html`, the `webR.read()` reader loop); the handler predates this
   branch and the diff does not touch it, so it is pre-existing rather than
   introduced. Disposition: **to the maintainer at the gate** — one line
   (`abandonBoot(...)` in that handler) closes it, and it is the same hazard
   class this milestone is about.
2. The losing branch of the install `Promise.race` has no `.catch`, so an
   install that rejects after the timeout raises an unhandled promise rejection.
   — Confirmed by reading; controls stay off, so console noise, not a state bug.
   Disposition: **to the maintainer at the gate**.
3. An install that rejects *before* the timeout propagates to the top-level
   catch, which says "Could not start R" though R started; `bootAbandoned` stays
   false and the two-minute timer is never cleared. — Confirmed; safe only
   because `#controls` is still hidden there. Disposition: **to the maintainer
   at the gate**.
4. The timeout races only `installPackages`, not `import(WEBR_URL)` or
   `webR.init()`, so a stall in the webR download still hangs on "Starting R in
   your browser…". — Correct, and outside AC2 and the milestone's In scope,
   which name the `installPackages` call. Disposition: **follow-up candidate**.
5. If `evalRString`/`evalRBoolean` rejects, the version guard throws to the
   top-level catch instead of refusing through `abandonBoot()`. — Confirmed;
   page is safe because `#controls` is still hidden. Disposition: **to the
   maintainer at the gate**.
6. The refusal messages point at jmgirard.github.io/hitop, which serves the
   prebuilt instruments rather than another builder page. — Wording judgment;
   AC1 asks the message to name where to get a working page and it names a
   destination. Disposition: **to the maintainer at the gate**.
7. The page says "120 seconds" while `README.md` says "two minutes". —
   Confirmed; same value, two forms, and AC4 pins only the minimum.
   Disposition: **reject, style**.
8. `installedIsOlderThan` leaves `.installed_version` and `.minimum_version`
   bound in R's global environment. — Confirmed, and it matches how the page
   binds every other value. Disposition: **reject, convention-conforming**.

**[S] blame-history (Sonnet):** one finding — `abandonBoot()`'s blanket disable
reaches the handover button that M062 deliberately kept independent of selection
state, though it is inert because `abandonBoot()` only fires during boot, before
any descriptor exists. Confirmed by reading; disposition: **reject, no
collision reachable**. The lens found no undone intent elsewhere: the
`packageVersion`-before-`library()` reorder has no prior deliberate ordering to
contradict, and the `bootAbandoned` guards are additive.

**[S] prior-review record (Sonnet):** "no prior-review evidence" — the M045,
M062, M063 and M066 archived `## Review` findings on these files are all still
satisfied, and both `gh api .../pulls/comments` probes returned empty, so no
per-PR walk was paid for. Zero findings.

**Return floor.** No finding demonstrates an acceptance criterion failing inside
its named procedure's domain: AC1 and AC2 were probed and passed, and finding 4
falls outside AC2's stated `installPackages` domain. Status stays `review`.

### Fix-now work directed at the gate (2026-08-29)

The maintainer chose to fix four of the five findings put to the gate before
merging, and to leave finding 6's wording as it stands. `hitop-builder` commit
`3d91721` on `m064-version-guard`:

- Finding 1 — the lost-connection handler now calls `abandonBoot()` instead of
  disabling the download buttons directly.
- Findings 2 and 3 — the install branch of the race carries its own rejection
  handler, so a rejection arriving after the timeout is logged and dropped
  rather than left unhandled, and a rejection arriving before it refuses
  through `abandonBoot()` saying the package download failed, instead of
  reaching the top-level catch as "Could not start R".
- Finding 5 — the version read and the comparison sit in a `try`, and a failure
  in either refuses through `abandonBoot()`.

Re-verified in the browser pane against the fixed page, probes deleted
afterwards:

- Channel loss, discriminating: with one scale selected the build button was
  enabled (so the probe could have failed); forcing `webR.read()` to reject
  latched the page — status "Lost the connection to R — reload the page to
  start over.", `#controls` hidden — and two further selection changes, the
  exact path that used to re-enable it, left zero enabled buttons.
- Forced install rejection: status "The hitop package could not be downloaded,
  so this page cannot build anything. …", log line
  `installPackages("hitop") failed: probe: forced install failure`, controls
  hidden, zero enabled buttons, and no `unhandledrejection` events.
- Version refusal (declared 99.0.0) and the timeout pair both still behave as
  recorded above: refusal message names 99.0.0 and 0.2.0, timeout latched the
  page and the abandoned install settled 4,589 ms later with every control
  still disabled; no `unhandledrejection` events in either.
- Happy path unaffected: declared 0.2.0 against installed 0.2.0 reached
  "Ready." with 78 scale checkboxes, and the seven comparison pairs returned
  the same values as before the fix.

