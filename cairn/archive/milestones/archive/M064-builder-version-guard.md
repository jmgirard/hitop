# M064: The builder refuses loudly when the package it installs no longer matches it

**Status:** done (2026-08-29, PR #71 https://github.com/jmgirard/hitop/pull/71 · hitop-builder PR #10 https://github.com/jmgirard/hitop-builder/pull/10)

**Goal:** Stop the builder page breaking mid-build when r-universe serves a `hitop` it no longer matches, and stop it waiting forever on an install that never finishes.

**Outcome:** `hitop-builder/index.html` declares `MIN_HITOP = '0.2.0'` and `INSTALL_TIMEOUT_MS = 120000` beside the other boot constants. The install is raced against the timeout; the installed version is read before `library()` and compared by `installedIsOlderThan()`, which binds both versions into R and tests `numeric_version(a) < numeric_version(b)`. Any refusal calls `abandonBoot()`, which sets a `bootAbandoned` latch, hides `#controls` and disables the download and handover buttons; every site that turns a control back on reads the latch, so a late-settling install cannot re-enable one. The latch also covers a lost R channel, a failed install and a version the page cannot read. `README.md` states the minimum and the timeout; this repo's `cairn/PROFILE.md` release-walk gained a Downstream step naming the builder, the six package surfaces it calls, and what a release must update there. No package code, data or docs changed.

**Decisions:** none cross-cutting. The milestone settled `MIN_HITOP` at the current released version and the timeout at six times a normal first load.

**Review:** three-lens fan-out; nine findings, none failing a criterion. Four robustness gaps fixed at the gate (`3d91721`) and re-probed, including a discriminating channel-loss probe; the untimed webR runtime download deferred; the refusal messages' destination accepted as it stands (DESIGN Known issues); three rejected. A probing technique joined the browser-pane lesson.
