# M064: The builder refuses loudly when the package it installs no longer matches it

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m064-builder-version-guard` (hitop) · `m064-version-guard` (hitop-builder)

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

- [ ] AC1 The page declares a minimum `hitop` version and compares it against the
      installed version with `numeric_version`, not string comparison; below the
      minimum the page stops with a message naming the declared minimum, the
      installed version and where to get a working page, and no download control
      is enabled. Probed with the installed version forced below, equal to and
      above the declared minimum, and on a pair whose components differ in digit
      count (`0.9.0` against `0.10.0`), so the comparison is exercised in both
      directions and in both version forms.
- [ ] AC2 A stalled install does not hang and cannot un-stall itself: the
      `installPackages` call is raced against a stated timeout; on timeout the
      status line says the install did not finish and what to try, and the
      download controls stay disabled. Probed twice — a timeout that fires with
      the install still pending, and a timeout that fires before an install which
      then settles — with the controls asserted still disabled in both.
- [ ] AC3 `cairn/PROFILE.md`'s release-walk slot carries a step naming
      `jmgirard/hitop-builder` and what a `hitop` release must update there.
- [ ] AC4 `README.md` states the declared minimum in exactly one prose location
      and says `index.html` is where it is declared. The sample descriptor's
      `packageVersion` is excluded by name, since it records the version that
      wrote a file rather than a minimum.
- [ ] AC5 The change ships: a merged pull request in `jmgirard/hitop-builder`
      whose URL is in this file's header, and the page served at
      `https://jmgirard.github.io/hitop-builder/` matches that commit's
      `index.html` byte for byte.
- [ ] AC6 The `hitop` package's code, tests, data and documentation are
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
- [ ] T6 Open the builder pull request; after merge, fetch the deployed page and
      compare bytes; write the URL into the header.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier); returned 3 findings on this milestone — a one-direction version probe blind to a lexical comparison getting `0.10.0` and `0.9.0` backwards, an unprobed hazard where an uncancellable install settles after the timeout and re-enables the controls, and an unsatisfiable "appears in one place" over a README carrying a version in two different roles — all fixed before the criteria were written.
- 2026-08-29: T1/T2 — `index.html` declares `MIN_HITOP = '0.2.0'` and `INSTALL_TIMEOUT_MS = 120000` beside the other boot constants; the install is raced against the timeout and the installed version is read before `library()` and compared by `installedIsOlderThan()`, which binds both versions into R as data and tests `numeric_version(a) < numeric_version(b)`. Either failure calls a new `abandonBoot()`, which sets a `bootAbandoned` latch, hides the controls and disables the download and handover buttons; the latch is also read at the three sites that turn a control back on, so an install settling after the timeout cannot re-enable one. T1 refined against AC1: `numeric_version`, not the `utils::compareVersion` the task offered as an alternative, since AC1 names `numeric_version` and criteria outrank task wording.
- 2026-08-29: T4 — six probes driven in the browser pane against `http://localhost:8788` serving copies of `index.html` with one constant altered each, deleted afterwards. Version: the real `installedIsOlderThan()` returned `TRUE` for 0.1.0<0.2.0 and 0.9.0<0.10.0, `FALSE` for 0.2.0<0.2.0, 0.3.0<0.2.0 and 0.10.0<0.9.0 — the last two pairs are the ones a string comparison gets backwards, so the check is shown able to fail the way it claims to catch. End to end: installed 0.2.0 against a declared 0.2.0 and 0.1.0 both reached "Ready." with the controls shown and 76 scale rows rendered; against a declared 99.0.0 the page stopped with a status naming 99.0.0, 0.2.0 and jmgirard.github.io/hitop, `#controls` hidden and `downloadBtn`/`saveDescriptor` both disabled. Timeout: with `INSTALL_TIMEOUT_MS = 1` the timeout fired at 1992 ms with the install still pending and the page stopped with both buttons disabled; the same install then settled at 9537 ms, 7.5 s after the timeout, with `bootAbandoned` already true and both buttons disabled at that instant, and both still disabled 30 s later.
- 2026-08-29: T5 — `README.md`'s "How it works" paragraph rewritten: it states the declared minimum (0.2.0) in one prose location, names `index.html` as where `MIN_HITOP` is set, and explains the component-wise comparison and the refusal; a second paragraph states the install timeout and its constant. A sentence under the sample descriptor says its `packageVersion` is the version that wrote that file, not the minimum. The other two `version` mentions in the file are a webR link and a scale-definitions aside, neither a version number.
- 2026-08-29: T3 — `cairn/PROFILE.md`'s release-walk slot gained a "Downstream" step naming `jmgirard/hitop-builder`, the six package surfaces the page calls, and the two things a release touching one of them must update there (`MIN_HITOP` in `index.html`, the minimum stated in that repo's `README.md`). File is 110 lines against the 120-line cap.
- 2026-08-29: gate chose `MIN_HITOP = '0.2.0'` (the current released version, and the one every page-called surface is present in) and a 120-second install timeout (six times the README's stated twenty-second first load).
- 2026-08-28: plan chose a declared minimum plus a release-walk step over a release-walk step alone (rejected: it keeps the page correct only for as long as nobody forgets, and the failure it misses is silent mid-build) and over a hard version pin (rejected as unavailable — r-universe serves only its current build and the install call takes no version); falsified by r-universe gaining versioned installs, which would make a pin the better answer.

## Decisions

## Review
