# M057: Each scale in the browser builder shows its clinician definition

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP3
- **Branch/PR:** `m057-builder-scale-definitions`

## Goal

A visitor choosing scales in the browser module builder can read each scale's
clinician-facing definition without leaving the picker, taking the text from the
installed package rather than a copy kept in the page.

## Scope

Surface tier: **user-facing** — the deliverable is the public builder page plus an
exported function's return value.

**In:** `available_scales("hitopsr")` gains a `Brief` column carrying each scale's
clinician-facing definition, joined to `hitopsr_definitions` on a camelCase stem the
definitions table gains in `data-raw/hitopsr_info.R`. The builder page
([jmgirard/hitop-builder](https://github.com/jmgirard/hitop-builder), one
`index.html`) reveals that definition per scale row on pointer hover and on keyboard
focus, tied to the checkbox by `aria-describedby`, dismissible with Escape, and
renders no description affordance at all when the installed package returns no such
column. NEWS, `?available_scales`, `?hitopsr_definitions`, builder README.

**Out:**
- The client-facing definition text (30-40 words) — stays where
  `vignettes/articles/scales-hitopsr.Rmd` already publishes it → candidate row.
- Subscale definitions (17 rows) — the builder picks scales, not subscales →
  candidate row.
- Reconciling the one scale whose display labels differ between the two tables
  (`NSSI` / `Non-suicidal Self-injury`) → M058, which this milestone does not depend
  on: the stem join is correct either way.
- Definitions for any instrument but the HiTOP-SR — `available_scales()` supports
  only `"hitopsr"`.

## Acceptance criteria

- [ ] AC1 `available_scales("hitopsr")` returns the three pre-existing columns plus a
      `Brief` column, joined on the camelCase stem rather than on a printed display
      name. A test walks every row it returns, comparing each row's `Brief` against
      the string `hitopsr_definitions` carries for that stem, the expected side read
      from `hitopsr_definitions` independently of `available_scales()`. Shown red
      under an altered definition value, a dropped definitions row, and a stem
      re-paired to a neighbouring scale.
- [ ] AC2 No scale is silently lost in the join: the same test asserts the set of
      `camelCase` stems returned equals the set `hitopsr_scales` carries — an
      equality over both tables, not a walk over whatever survived — and that no
      returned `Brief` is `NA` or empty. Both expected sides read from the shipped
      tables, never hardcoded in the test file.
- [ ] AC3 With definitions available, every rendered scale row shows that row's own
      definition: a browser-driven check queries all rendered rows, resolves each
      checkbox's `aria-describedby` to its target, and asserts that target's text
      equals the `Brief` the page's own `available_scales()` call returned for that
      row. The queried row count is asserted equal to the number of rows that call
      returned; the check is shown red under a page mutation rotating the
      definitions by one row.
- [ ] AC4 Reachable by pointer and keyboard, dismissible from both: on three named
      scales, a browser-driven run shows the definition on pointer hover, shows the
      same string after reaching the checkbox by real Tab presses, and shows it
      hidden after Escape in each of those two states. This verifies transport only;
      AC1 pins the package side.
- [ ] AC5 Driven against a stubbed `available_scales()` result carrying only the
      three pre-existing columns, the page renders one row per stub row, ticking a
      box still updates the tally, and no row carries an `aria-describedby` or a
      hover trigger at all — the degraded behavior is the absence of the affordance,
      not an empty or placeholder description.
- [ ] AC6 The page holds no copy of the definition text: a check over the built
      `index.html` asserts none of the 76 `Brief` strings occurs in its source. NEWS
      names the new column and the page behavior; `?available_scales` lists all four
      returned columns (its roxygen today hardcodes "three columns",
      `R/available_scales.R:13-15`); the builder README says definitions come from
      the installed package.
- [ ] AC7 `devtools::document()` no diff, `devtools::test()` clean,
      `devtools::check()` clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T2, T3, T4
- AC2 → T1, T2, T4
- AC3 → T5, T6
- AC4 → T5, T6
- AC5 → T5, T7
- AC6 → T5, T8
- AC7 → T9

## Tasks

- [x] T1 Add a `camelCase` column to `hitopsr_definitions` in
      `data-raw/hitopsr_info.R:45-48`, derived as `hitopsr_scales$camelCase` is
      (`data-raw/hitopsr_info.R:18`), with the one differing label paired explicitly
      in the script and the pairing commented; regenerate the `.rda`. Update
      `?hitopsr_definitions` (`R/data.R:170-184`) for the new column.
- [x] T2 Write the AC1/AC2 test first and confirm it red: stem-set equality against
      `hitopsr_scales`, per-row `Brief` comparison read independently.
- [x] T3 Add the `Brief` column to `available_scales()` (`R/available_scales.R:27-38`)
      joining on the stem; update the `@return` roxygen and `document()`.
- [x] T4 Run the three AC1 mutations (altered value, dropped row, re-paired stem);
      confirm each red, restore, confirm green.
- [x] T5 In the builder repo: render a description element per scale row carrying
      that row's `Brief`, wired by `aria-describedby`, shown on hover and on focus,
      hidden on Escape; render no affordance when the column is absent
      (`index.html:674-700` `renderScales()`, `index.html:961-970` the data read).
- [x] T6 Drive the built page for AC3 and AC4: all-row pairing check with the
      rotate-by-one mutation, then the three-scale hover/Tab/Escape run.
- [x] T7 Drive the page against a stubbed three-column `available_scales()` for AC5.
- [x] T8 AC6: the no-embedded-copy check over `index.html`, the NEWS entry, the
      builder README line.
- [x] T9 `document()`/`test()`/`check()`; open both PRs. **Merge order is the
      package first**, then the builder once r-universe has rebuilt — the reverse of
      M056, because the page's new behavior needs the new column to exist. AC5 is
      what makes the interval safe.

## Work log

- 2026-08-26: created by /milestone-plan.
- 2026-08-26: criteria audit ran in full mode (user-facing tier), fresh-context [O] reader. Returned findings on all six drafted criteria; all had one clear right answer and were fixed before the gate — key resolution and column name named in AC1; total-coverage equality added to AC2 (the old wording quantified over "rows returned", vacuous exactly under a drop); AC3 raised from presence to per-row pairing; the AC3/AC5 contradiction resolved by scoping AC3 and naming AC5's degraded behavior; AC4's Escape bound to both states; AC6's no-embedded-copy property asserted rather than only documented. No finding went to the gate. The gate changed no criterion, so no re-audit was owed.
- 2026-08-26: plan gate chose the package-side column over a page-side lookup because the name join then lives beside both tables and under test, where a second repo's JavaScript would drift — the reasoning D-035 used against rebuilding packaging logic app-side; falsified by evidence that an r-universe rebuild lag leaves the page without definitions long enough to matter, which AC5's degraded path would then have to become permanent.
- 2026-08-26: plan gate chose a hover-and-focus tooltip over a bare `title` attribute, an always-visible second line, and a tap-to-expand button, because it honors the request while reaching keyboard and screen-reader users, and the definitions run to 118 characters, which would roughly triple the height of an already-scrolling 76-row list; falsified by a visitor reporting the popup unreachable or obstructive on touch, which points at the always-visible option.
- 2026-08-26: plan gate chose to match the two tables on the camelCase stem over hard-coding the one differing label and over renaming a label, because the stem is already the app's own value and no printed name moves; falsified by a second stem collision, which would make the derived stems no better a key than the names.
- 2026-08-26: implementation gate, three choices, all as recommended: a definitions row's stem names whatever that row defines (the subscale where there is one, else the scale), so the column keys all 93 rows and pairs with `hitopsr_subscales` as well as `hitopsr_scales`; `data-raw/hitopsr_info.R` stops on a stem-set disagreement with either table rather than only stating the one label pairing, so a relabelled scale cannot rebuild a table with an unmatched stem; and the builder's hover target is the whole scale row with a short delay, not a per-row info marker, so nothing new must be found or aimed at.
- 2026-08-26: T1 done. `hitopsr_definitions` gains `camelCase` (93 rows, 5 columns); the "Non-suicidal Self-injury"/"NSSI" pair is stated in the script and the three set-equality guards pass. Regenerating the script left the other three HiTOP-SR `.rda` files byte-identical. `document()` wrote `man/hitopsr_definitions.Rd`; `test()` 14,419 pass / 0 fail / 1 skip.
- 2026-08-26: T2 done. Two tests added to `test-available_scales.R`: the per-row `Brief` walk with stem-set equality against both `hitopsr_scales` and the definitions table, and a guard that the definitions table keys on a stem rather than a printed name. Confirmed red before T3 (76 pairing failures plus the column-name assertion).
- 2026-08-26: T3 done. `available_scales()` returns `Scale`/`camelCase`/`nItems`/`Brief`; the internal `scale_definitions()` matches on the stem and aborts with class `hitop_missing_definition` rather than returning a hole, fired directly in a test with a passing control over all 76 shipped stems. `@return` roxygen now names four columns.
- 2026-08-26: T4 done. AC1's three mutations were planted in the join, not in `hitopsr_definitions`: the test reads its expected side from that table (AC2 forbids hardcoding one), so a data-side edit moves both sides together and is invisible by construction — verified by running all three data-side first, where only the dropped row went red, and only because the new guard aborts. Planted join-side, each was red and each restore green: one `Brief` altered on the way out (red at the per-row walk), a definitions row dropped before the join (red as the guard abort), a scale dropped from the returned tibble (red at the row-count and set equalities), and every stem re-paired to its neighbour (red at the per-row walk). `test()` 14,583 pass / 0 fail / 1 skip.
- 2026-08-26: T5 done (jmgirard/hitop-builder, branch `m057-scale-definitions`, commit 389050a). Each scale row gets a `.desc` element carrying that row's `Brief`, `aria-describedby` from the checkbox, shown on `pointerenter` after 300 ms and on checkbox focus, hidden on Escape with a dismissal flag so Escape is visible while the pointer or focus that showed it has not moved. `position: fixed` keeps it out of the scale list's own `overflow-y: auto` clip; `pointer-events: none` keeps it from swallowing the row beneath. `definitionsAvailable()` gates the whole affordance all-or-nothing. `applyFilter()` now reads the name span by class rather than by being the first `span`, since the description is a sibling.
- 2026-08-26: T6/T7 driven against a local server. Two harness copies of `index.html` were used, each differing from the shipped file in two named places: the R expression (the branch's four-column `available_scales()` is not on r-universe, so the frame is rebuilt from the installed package's own tables — AC4 scopes these two criteria to transport, with AC1 pinning the package side) and one line exposing the frame the page received (the shipped page is an ES module, so its top-level bindings are unreachable from a driving script).
- 2026-08-26: AC3 verified. 76 rows queried, 76 rows in the frame the page received, counts equal; every checkbox's `aria-describedby` resolved and every target's text matched that row's `Brief` — 0 mismatches. Rotating the 76 description texts by one row in the DOM produced 76 mismatches; restoring gave 0 again.
- 2026-08-26: AC4 verified on Agoraphobia, Low Sexual Arousal and Workaholism. For each: real pointer hover showed that scale's definition (id and text checked, not presence); Escape hid it; three real Tab presses from the filter box put focus on that scale's checkbox and showed the same string; Escape hid it again. Both Escape states checked on all three.
- 2026-08-26: AC5 verified twice. Against a deliberate three-column stub of `available_scales()`: 76 rows rendered against 76 rows returned, zero `.desc` elements, zero `aria-describedby`, no popup after two seconds of real hover on a row, and a real click ticked Agoraphobia and moved the tally to "1 of 76 scales selected — 5 items." Against the shipped `index.html` unmodified with the published hitop 0.2.0, which returns three columns today: 76 rows, zero `.desc`, zero `aria-describedby` — the actual state of the deployed page during the merge interval.
- 2026-08-26: T8 done. AC6's no-embedded-copy check run over the built `index.html` (48,588 bytes): none of the 76 `Brief` strings occurs in it, with a passing control on a string the page does have and a planted control showing the check goes red when one `Brief` is pasted in. NEWS gained three entries (the `Brief` column and its stem join, `hitopsr_definitions`'s new column, the page behavior); the builder README's *Choose scales* step now describes the popup and says the text comes from the installed package.
- 2026-08-26: T9. `check()` first returned one NOTE of this branch's own making — `hitopsr_definitions` referenced by name inside a function was not in the `utils::globalVariables()` list `R/hitop-package.R` keeps for the other ten lazy-loaded datasets. Fixed rather than justified: the dataset is declared there, and the `switch()` reading it became `module_definition_tables()`, a sibling of `module_scale_tables()` so a future instrument with a scale table but no definitions is a missing key rather than a column of NA. `document()` no diff, `pkgdown::check_pkgdown()` no problems, `check()` 0 errors / 0 warnings / 0 notes.

## Decisions

## Review
