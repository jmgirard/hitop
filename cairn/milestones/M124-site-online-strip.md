# M124: The instrument download pages keep three cards per row and gain an online-form strip, and the site lists the steps from instrument to scores

- **Status:** review
- **Priority:** normal
- **Depends on:** M125
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the pkgdown site's download pages, navbar and overview page
- **Branch/PR:** `m124-site-online-strip`

## Goal

Return each download page's card row to three cards, give the online route a strip under the row that holds the study-link button and the JSON export, reach the link builder from the Instruments menu, and write the four steps from instrument to scores on the overview page.

## Scope

**In:** `vignettes/articles/_download-helpers.R` gains a strip renderer with a base64url encoder; the five download pages with a JSON export drop the JSON card and add the strip; `pkgdown/extra.css` styles it; `_pkgdown.yml`'s Instruments menu gains "Make a study link"; `overview.Rmd` gains the step list; `online-collection.Rmd`'s "Make the study link" section gains one sentence on a module arriving from the builder; tests; NEWS. D-033 governs the strip's JSON anchor: same-origin href, `download` attribute, build badge.

**Out:** `link.html` reading the parameter → M125 (this milestone waits on it). The builder's card → M126. The HSUM page: no JSON export and no form support, so no strip. A per-language download layout → the multi-language candidate row.

## Acceptance criteria

- [x] AC1: For each of the six download pages, evaluating its `downloads` chunk (extracted with `knitr::purl()`) after sourcing the helpers renders exactly three `col-md-4` cards inside `.hitop-downloads`. A test in `tests/testthat/` shows this.
- [x] AC2: The same evaluation renders, for each of the five pages with a JSON export, exactly one `.hitop-online` element under the row, and for `download-hitophsum.Rmd` none. The element holds an anchor to `https://jmgirard.github.io/hitop-form/link.html?c=<p>`, where `<p>` is the unpadded base64url encoding of `{"instrument":"<stem>"}` as hitop-form's `encodeConfig` writes it and the test decodes `<p>` back to the page's stem (`hitopsr`, `hitopbr`, `pid5`, `pid5sf`, `pid5bf`); an anchor to `../articles/online-collection.html`; and an anchor to `../downloads/<stem>.json` carrying `download="<stem>.json"` and a build badge. The "every rendered download button carries a download attribute" test in `test-artifacts.R` covers the strip renderer.
- [x] AC3: Each of the five pages writes its JSON `dl_link()` call literally in its Rmd, the test "download-page links point at the staged site copies" in `tests/testthat/test-artifacts.R` passes unchanged, and each page's rendered chunk carries an anchor to every file in its `hitop_artifacts` rows. A test shows the last part.
- [x] AC4: A test reads `_pkgdown.yml` and asserts an Instruments entry "Make a study link" with href `https://jmgirard.github.io/hitop-form/link.html` directly after "Build a HiTOP-SR Module". `overview.Rmd` carries an ordered list of four items whose text contains, in order, "Choose an instrument", "Choose a route", "Collect responses" and "Read and score", and a test resolves every site-relative href in that list to a file under `vignettes/articles/`, `vignettes/` or `man/`.
- [ ] AC5: `devtools::check()` returns 0 errors, 0 warnings and 0 notes, `pkgdown::check_pkgdown()` is clean, `pkgdown::build_site()` renders the six download pages and the overview page without error, and `NEWS.md` carries an entry naming the strip and the step list.

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T4

## Tasks

- [x] T1: In `_download-helpers.R` add `online_strip(instrument, stem, json_link)` rendering the `.hitop-online` element with the three anchors (the JSON one through the existing `render_button` path, `_download-helpers.R:64-79`), a base64url encoder over `jsonlite::base64_enc()`, and the strip's CSS in `pkgdown/extra.css`.
- [x] T2: Edit the five pages (drop the JSON `dl_card()`, call the strip with a literal `dl_link()`), the `_pkgdown.yml` entry after `_pkgdown.yml:38`, the `overview.Rmd` step list with its links, and the `online-collection.Rmd` sentence.
- [x] T3: Tests for AC1 to AC4 in a new `tests/testthat/test-download-pages.R`, and extend the download-attribute test in `test-artifacts.R` to the strip renderer.
- [x] T4: NEWS entry; `build_site()`, `check_pkgdown()`, `check()` (the PR is opened by `/milestone-review`).

## Work log

- 2026-09-24: created by /milestone-plan.
- 2026-09-24: criteria audit ran in full mode (fresh [O] reader); its M124 repairs are written here (rendered card counts over source greps, the strip's `download` attribute and badge, unpadded base64url, the literal `dl_link()` and the rendered-anchor check, reference hrefs allowed, a test over the navbar).
- 2026-09-24: plan gate chose an online-form strip under the cards over a quiet JSON line alone, because the online route is a fielding route like the other three and needs a home on the page; falsified by a visitor report that the strip reads as a fourth card.
- 2026-09-24: plan gate chose the strip over four narrower cards because the three-card row is the intended design (M094's review had rejected the wrap as cosmetic); falsified by nothing short of a design change.
- 2026-09-24: plan gate chose the overview step list plus the menu entry over the menu entry alone, because the tutorial was the only written sequence; falsified by the overview page growing past one screen.
- 2026-09-25: /milestone-implement started; branch `m124-site-online-strip` cut from pushed main. hitop-form PR #11 (the `c` reader the strip's link relies on) merged 2026-09-25.
- 2026-09-25: question gate: the strip carries a title, one sentence and three links (over title and links alone); the AC4 test reads `_pkgdown.yml` with {yaml}, added to Suggests (over a line-based parse; D-074); the overview step list sits below the cards under a heading (over above them). Step 1 links nothing, step 2 the two route tutorials, step 3 the online-collection sending section, step 4 `read_form_responses()` and the scoring tutorials.
- 2026-09-25: T1 code in place, checkpointed while the full suite still ran (the tick follows its result). `.button_renderer(instrument)` hoisted out of `download_cards()` so `online_strip()` renders the JSON button on the same path; `.base64url()` over `jsonlite::base64_enc()` (newlines stripped, `+/` to `-_`, padding dropped) checked equal to `encodeConfig()` in node for all five stems; `.hitop-online` CSS.
- 2026-09-25: T1 ticked on the full suite's clean run (FAIL 0, 15 pre-existing merge-base skips).
- 2026-09-25: T2 done. Five pages call `online_strip()` after `download_cards()` in the same `downloads` chunk with a literal `dl_link()`; menu entry; overview `<ol class="hitop-steps">` under a "From instrument to scores" heading, step 3 targeting the tutorial's `make-the-study-link` id read from the built site; the tutorial sentence names the download-page button and a `c` carrying a module rather than the builder card, which M126 has not built yet. A first plant run restored files with `git checkout` and wiped four uncommitted T2 edits, which were re-applied; plants were re-run with copy-based restores.
- 2026-09-25: T3 done. `test-download-pages.R`: five tests, 89 expectations, each page's `downloads` chunk purled and evaluated against the sourced helpers, `c` decoded back to the exact `{"instrument":"<stem>"}` text; the AC3 test also refuses a `../downloads/` href outside the page's manifest rows. `test-artifacts.R`'s download-attribute test renders the strip for each JSON row. Nine plants red, one each: wrong stem in the strip call, the pid5 page as on main, the menu entry above the module builder (first caught as an error, the test then reworked to fail on it), the menu href on the form page, a dead step link, steps swapped, a plain JSON anchor (both test files), the strip before the row, a strip on the HSUM page. Clean run 89 pass. Checkpointed while the full suite, `build_site()` and `check()` ran in the background; their results follow.
- 2026-09-25: minor amendment: T4's "open the PR and read its CI" clause is review-owned (the PR is opened at `/milestone-review` step 8 after the user's approval, D-138), so T4 ends at the NEWS entry and the three checks.
- 2026-09-25: T3 ticked on the full suite's clean run (FAIL 0, the same 15 merge-base skips). T4 done: NEWS entry under New features; `check_pkgdown()` no problems; `build_site()` finished, the six download pages and the overview rendered (built pages hold 3 cards each, one strip on five and none on HSUM, the overview `<ol>` with 4 items, the tutorial's `make-the-study-link` id present); `check()` 0 errors, 0 warnings, 0 notes. Browser pane on the built PID-5-SF page at 1280 px in both themes: three cards in one row, the bordered strip under it with the primary button, the badged JSON button and the quiet link; at 375 px the strip measures 351 px wide. A stale cached `extra.css` in the pane first hid the strip's border; a cache-bypassed load showed it.
- 2026-09-25: claim audit: 50 claims read, 2 corrected — vignettes/articles/_download-helpers.R (the strip comment said one sentence; the paragraph has two), vignettes/articles/overview.Rmd (step 2 offered the online form on every page; the HSUM page has none). Fresh [O] reader; each correction re-read once by the same reader.
- 2026-09-25: after the two corrections, `test-download-pages.R` (89), `test-artifacts.R` (137) and `test-overview.R` (9) pass and the overview article rebuilds with the corrected step. Status set to review; no PR yet.

## Decisions

## Review

Reviewed 2026-09-25 on `m124-site-online-strip` at ac62162f, main not moved since the branch was cut, no PR yet.

- AC1: `test-download-pages.R` "each download page renders exactly three cards in its row" passes, 13 expectations over the six pages (one row fence each, three `col-md-4` cards each; HSUM included). ✔
- AC2: "the five form-backed pages render one online strip and the HSUM page none" passes, 47 expectations: one `.hitop-online` fence after the row on five pages and none on HSUM; the `c` parameter matches `^[A-Za-z0-9_-]+$` and decodes (hitop-form's alphabet and padding restored) to exactly `{"instrument":"<stem>"}` for each stem; anchors to `../articles/online-collection.html` and to `../downloads/<stem>.json` with `download="<stem>.json"` and a build badge. `test-artifacts.R` "every rendered download button carries a download attribute" passes, 34 expectations, the five JSON rows rendered through `online_strip()`. ✔
- AC3: each of the five pages carries `online_strip(..., dl_link("English (JSON File)", "../downloads/<stem>.json"))` literally (grep, five hits); `git diff main..HEAD -- tests/testthat/test-artifacts.R` touches only the download-attribute test, and "download-page links point at the staged site copies" passes unchanged, 43 expectations; "each page's rendered chunk links every file in its manifest rows and no other" passes, 12 expectations (set equality per page). ✔
- AC4: "the Instruments menu reaches the link builder right after the module builder" passes, 3 expectations, `_pkgdown.yml` read with `yaml::read_yaml()`; "the overview page lists the four steps from instrument to scores with links that resolve" passes, 14 expectations (one `<ol>`, four `<li>` in the named order, every site-relative href resolved to a file under `vignettes/articles/`, `vignettes/` or `man/`). `test-overview.R` passes, 9 expectations. ✔
