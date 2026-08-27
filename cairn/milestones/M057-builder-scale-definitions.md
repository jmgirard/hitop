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
- [ ] T2 Write the AC1/AC2 test first and confirm it red: stem-set equality against
      `hitopsr_scales`, per-row `Brief` comparison read independently.
- [ ] T3 Add the `Brief` column to `available_scales()` (`R/available_scales.R:27-38`)
      joining on the stem; update the `@return` roxygen and `document()`.
- [ ] T4 Run the three AC1 mutations (altered value, dropped row, re-paired stem);
      confirm each red, restore, confirm green.
- [ ] T5 In the builder repo: render a description element per scale row carrying
      that row's `Brief`, wired by `aria-describedby`, shown on hover and on focus,
      hidden on Escape; render no affordance when the column is absent
      (`index.html:674-700` `renderScales()`, `index.html:961-970` the data read).
- [ ] T6 Drive the built page for AC3 and AC4: all-row pairing check with the
      rotate-by-one mutation, then the three-scale hover/Tab/Escape run.
- [ ] T7 Drive the page against a stubbed three-column `available_scales()` for AC5.
- [ ] T8 AC6: the no-embedded-copy check over `index.html`, the NEWS entry, the
      builder README line.
- [ ] T9 `document()`/`test()`/`check()`; open both PRs. **Merge order is the
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

## Decisions

## Review
