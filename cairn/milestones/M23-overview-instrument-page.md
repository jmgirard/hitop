<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M23: Overview instrument page (SR/BR/HSUM link cards)

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** GP3   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

A single beautified pkgdown article of summary link-cards for the three main
HiTOP instruments (SR, BR, HSUM), reusing the M21 card/theme design, embeddable
by URL and discoverable atop the Instruments navbar menu.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**
- New `vignettes/articles/overview.Rmd` with exactly three summary link-cards —
  HiTOP-SR, HiTOP-BR, HiTOP-HSUM — each carrying an emoji icon, the instrument
  name, its item/scale summary (numbers reused verbatim from the existing
  `download-*.Rmd` intros, not re-derived), a one-line description, and a
  prominent "View & download →" link to that instrument's existing download
  page (`download-hitopsr.html` / `download-hitopbr.html` /
  `download-hitophsum.html`).
- Reuse of the shared `.hitop-*` / Bootstrap-5-variable card styling in
  `pkgdown/extra.css` (light-switch-safe); the page opts into the existing
  full-width layout via the `.hitop-downloads` class. No inline `<style>`.
- `_pkgdown.yml`: overview added as the **first** entry of the "Instruments"
  navbar menu (label e.g. "All HiTOP Instruments").
- A test locking the overview page's presence and its three instrument-page
  links.
- NEWS.md entry.

**Out:**
- PID-5 Full/SF/BF cards → their own download pages own them (this page covers
  only the three main HiTOP instruments per the external target).
- Download buttons / M20 build-date badges on the overview itself → the
  per-instrument download pages own downloads; the overview links to them.
- iframe/embed markup on hitop-system.org → external, Jeff-controlled site;
  not this repo.
- Multi-language download UI → separate ROADMAP candidate (lineage M21).

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] **AC1** The built article `articles/overview.html` shows exactly three
      instrument cards — HiTOP-SR, HiTOP-BR, HiTOP-HSUM — each with its
      item/scale summary and a working link to the correct download page
      (`download-hitopsr.html`, `download-hitopbr.html`,
      `download-hitophsum.html`). Evidence: rendered HTML (card count + hrefs).
- [ ] **AC2** The page reuses the shared theme-aware `.hitop-*` styling with no
      inline `<style>`, and renders correctly in both light and dark pkgdown
      themes. Evidence: grep shows no `<style>` in the page/output; computed-style
      or screenshot probe in both light and dark.
- [ ] **AC3** The overview page is the first entry in the navbar "Instruments"
      menu and resolves to the article. Evidence: `_pkgdown.yml` diff + rendered
      navbar menu.
- [ ] **AC4** A test asserts the overview page exists and links exactly the three
      main-instrument download pages, and fails if a card link is broken or the
      instrument set changes. Evidence: test source + passing run + a mutation
      check (break one link → test fails).
- [ ] **AC5** NEWS.md carries a user-facing entry announcing the overview page.
      Evidence: NEWS.md diff.
- [ ] **AC6** Consistency gate clean: `pkgdown::check_pkgdown()` clean,
      `devtools::check()` 0/0/0, `devtools::test()` green, `cairn_validate` 0.
      Evidence: command output.

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number. Review reads to fence evidence. -->

- AC1 → T1, T5
- AC2 → T1, T5
- AC3 → T2, T5
- AC4 → T3
- AC5 → T4
- AC6 → T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate -->

- [ ] **T1** Author `vignettes/articles/overview.Rmd`: front matter
      (`toc: false`), a short intro sentence, and a three-card row for SR/BR/HSUM.
      Emit the cards through the shared `.hitop-*` card styling (either a small
      `instrument_cards()` helper in `vignettes/articles/_download-helpers.R` or a
      static `{=html}` card row consistent with the "Explore the R Package
      Features" blocks in `download-hitopsr.Rmd:51`), wrapped so the
      `.hitop-downloads` full-width layout and theme apply. Reuse the item/scale
      summary numbers verbatim from each `download-*.Rmd` intro.
- [ ] **T2** Add overview as the first entry in the "Instruments" navbar menu in
      `_pkgdown.yml` (above the six per-instrument entries).
- [ ] **T3** Add a test (new `test-overview.R` or a block in
      `tests/testthat/test-artifacts.R`) locking: `overview.Rmd` exists, links the
      three main-instrument download pages, and covers exactly those three.
- [ ] **T4** Add a NEWS.md entry for the overview page.
- [ ] **T5** Build the site (`build_articles()` / `init_site()`), verify the page
      renders with three cards + correct links in light and dark, confirm the
      navbar entry resolves, and run the consistency gate
      (`check_pkgdown()`, `check()`, `test()`, `cairn_validate`).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-17: created by /milestone-plan (absorbs the "Overview instrument page"
  candidate, lineage M21).

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md -->

## Review
<!-- owner: review · exclusive; evidence per criterion, consistency-gate
     results, review findings + triage. EXEMPT from the 150-line cap (M55). -->
