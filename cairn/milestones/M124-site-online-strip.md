# M124: The instrument download pages keep three cards per row and gain an online-form strip, and the site lists the steps from instrument to scores

- **Status:** planned
- **Priority:** normal
- **Depends on:** M125
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the pkgdown site's download pages, navbar and overview page
- **Branch/PR:** —

## Goal

Return each download page's card row to three cards, give the online route a strip under the row that holds the study-link button and the JSON export, reach the link builder from the Instruments menu, and write the four steps from instrument to scores on the overview page.

## Scope

**In:** `vignettes/articles/_download-helpers.R` gains a strip renderer with a base64url encoder; the five download pages with a JSON export drop the JSON card and add the strip; `pkgdown/extra.css` styles it; `_pkgdown.yml`'s Instruments menu gains "Make a study link"; `overview.Rmd` gains the step list; `online-collection.Rmd`'s "Make the study link" section gains one sentence on a module arriving from the builder; tests; NEWS. D-033 governs the strip's JSON anchor: same-origin href, `download` attribute, build badge.

**Out:** `link.html` reading the parameter → M125 (this milestone waits on it). The builder's card → M126. The HSUM page: no JSON export and no form support, so no strip. A per-language download layout → the multi-language candidate row.

## Acceptance criteria

- [ ] AC1: For each of the six download pages, evaluating its `downloads` chunk (extracted with `knitr::purl()`) after sourcing the helpers renders exactly three `col-md-4` cards inside `.hitop-downloads`. A test in `tests/testthat/` shows this.
- [ ] AC2: The same evaluation renders, for each of the five pages with a JSON export, exactly one `.hitop-online` element under the row, and for `download-hitophsum.Rmd` none. The element holds an anchor to `https://jmgirard.github.io/hitop-form/link.html?c=<p>`, where `<p>` is the unpadded base64url encoding of `{"instrument":"<stem>"}` as hitop-form's `encodeConfig` writes it and the test decodes `<p>` back to the page's stem (`hitopsr`, `hitopbr`, `pid5`, `pid5sf`, `pid5bf`); an anchor to `../articles/online-collection.html`; and an anchor to `../downloads/<stem>.json` carrying `download="<stem>.json"` and a build badge. The "every rendered download button carries a download attribute" test in `test-artifacts.R` covers the strip renderer.
- [ ] AC3: Each of the five pages writes its JSON `dl_link()` call literally in its Rmd, the test "download-page links point at the staged site copies" in `tests/testthat/test-artifacts.R` passes unchanged, and each page's rendered chunk carries an anchor to every file in its `hitop_artifacts` rows. A test shows the last part.
- [ ] AC4: A test reads `_pkgdown.yml` and asserts an Instruments entry "Make a study link" with href `https://jmgirard.github.io/hitop-form/link.html` directly after "Build a HiTOP-SR Module". `overview.Rmd` carries an ordered list of four items whose text contains, in order, "Choose an instrument", "Choose a route", "Collect responses" and "Read and score", and a test resolves every site-relative href in that list to a file under `vignettes/articles/`, `vignettes/` or `man/`.
- [ ] AC5: `devtools::check()` returns 0 errors, 0 warnings and 0 notes, `pkgdown::check_pkgdown()` is clean, `pkgdown::build_site()` renders the six download pages and the overview page without error, and `NEWS.md` carries an entry naming the strip and the step list.

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T4

## Tasks

- [ ] T1: In `_download-helpers.R` add `online_strip(instrument, stem, json_link)` rendering the `.hitop-online` element with the three anchors (the JSON one through the existing `render_button` path, `_download-helpers.R:64-79`), a base64url encoder over `jsonlite::base64_enc()`, and the strip's CSS in `pkgdown/extra.css`.
- [ ] T2: Edit the five pages (drop the JSON `dl_card()`, call the strip with a literal `dl_link()`), the `_pkgdown.yml` entry after `_pkgdown.yml:38`, the `overview.Rmd` step list with its links, and the `online-collection.Rmd` sentence.
- [ ] T3: Tests for AC1 to AC4 in a new `tests/testthat/test-download-pages.R`, and extend the download-attribute test in `test-artifacts.R` to the strip renderer.
- [ ] T4: NEWS entry; `build_site()`, `check_pkgdown()`, `check()`; open the PR and read its CI.

## Work log

- 2026-09-24: created by /milestone-plan.
- 2026-09-24: criteria audit ran in full mode (fresh [O] reader); its M124 repairs are written here (rendered card counts over source greps, the strip's `download` attribute and badge, unpadded base64url, the literal `dl_link()` and the rendered-anchor check, reference hrefs allowed, a test over the navbar).
- 2026-09-24: plan gate chose an online-form strip under the cards over a quiet JSON line alone, because the online route is a fielding route like the other three and needs a home on the page; falsified by a visitor report that the strip reads as a fourth card.
- 2026-09-24: plan gate chose the strip over four narrower cards because the three-card row is the intended design (M094's review had rejected the wrap as cosmetic); falsified by nothing short of a design change.
- 2026-09-24: plan gate chose the overview step list plus the menu entry over the menu entry alone, because the tutorial was the only written sequence; falsified by the overview page growing past one screen.

## Decisions

## Review
