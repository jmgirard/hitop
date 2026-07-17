<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M21: Beautify instrument download pages

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate -->
- **Depends on:** —   <!-- M20 shipped; lineage only -->
- **Principles touched:** GP2   <!-- works under: version info stays manifest-driven and user-visible (D-016) -->
- **Branch/PR:** m21-beautify-download-pages   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create -->

Redesign the six pkgdown instrument download pages so build-date version info is visible at the point of download and the M20 manifest tables become a polished, theme-aware, collapsible versions section.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**
- All 6 `vignettes/articles/download-*.Rmd` pages: whole-page visual polish (spacing, card refinement, section headers) plus the Versions-section redesign.
- Version treatment (gate-approved 2026-07-16): build-date badges on/near each download button; full current-builds manifest (incl. MD5) and version history rendered as custom HTML from `hitop_artifacts` inside a styled collapsible (`<details>`) section.
- Deduplication: shared styles move to `pkgdown/extra.css`; version-section rendering moves to one shared helper under `vignettes/articles/` sourced by all 6 pages (not in `R/` — no package-namespace pollution).
- Both site themes supported (`light-switch: true`): styling verified in light and dark.
- Prototype-first sequencing: PID-5 page approved by Jeff before rollout to the other 5 (HSUM's 3-card variant handled in rollout).
- (Amendment 2026-07-16, prototype checkpoint): manifest wording cleanup — reword the `hitop_artifacts` `changes` text so public pages carry no internal tracking IDs; `data/hitop_artifacts.rda` re-derived wording-only via `data-raw/artifacts.R` (dates and MD5s identical, no artifact bytes change).

**Out:**
- Artifact regeneration — no `inst/extdata/` bytes change (that machinery is M20, done).
- Multi-language download UI (scaling beyond per-language buttons) — ROADMAP candidate, added 2026-07-17.
- Scoring-tutorial article styling and homepage/README polish — not requested; add a candidate row later if wanted.
- Any change to exported functions or their docs — no runtime surface in this milestone.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1: Each of the 6 download pages shows a build-date indicator at/near each download button, sourced from `hitop_artifacts` at render time — no hardcoded dates in page source (grep evidence + rendered-page check).
- [ ] AC2: Each page's Versions section renders the current-builds manifest (file, format, instrument version, build date, MD5) and full version history from `hitop_artifacts` inside a styled collapsible element (rendered-page evidence).
- [ ] AC3: No duplicated inline `<style>` blocks remain in `download-*.Rmd`; shared CSS lives in `pkgdown/extra.css` and version rendering in exactly one shared helper file (grep evidence).
- [ ] AC4: Pages render correctly in both light and dark themes (screenshots from locally built site, both themes).
- [ ] AC5: `devtools::test()` clean — in particular the download-page href lock in `tests/testthat/test-artifacts.R` — with no `inst/extdata/` changes; `data/hitop_artifacts.rda` changed only per the gated wording-only amendment (dates and MD5s identical).
- [ ] AC6: Prototype checkpoint honored: work-log line records Jeff's approval of the PID-5 page design before rollout commits to the other 5 pages.
- [ ] AC7: Local `pkgdown::build_site()` completes without errors and all 6 pages render.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T2, T3, T5
- AC2 → T2, T3, T5
- AC3 → T1, T2, T5
- AC4 → T3, T6
- AC5 → T6
- AC6 → T4
- AC7 → T6

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: Move the per-page inline `<style>` block (full-width main, hidden sidebar) into `pkgdown/extra.css`; add theme-aware styles for badges and the collapsible versions section using Bootstrap 5 CSS variables so both themes work.
- [x] T2: Write the shared version-section helper under `vignettes/articles/` (sourced R helper or child Rmd, parameterized by instrument) that emits build-date badge values and the collapsible current-builds + history HTML from `hitop_artifacts`.
- [x] T3: Prototype on `download-pid5.Rmd`: badges on download buttons, collapsible versions section, whole-page polish; `pkgdown::build_site()` locally; capture light + dark screenshots.
- [x] T4: Checkpoint — present screenshots to Jeff, iterate until approved, log approval in the work log.
- [x] T5: Roll the approved design out to the remaining 5 pages (pid5sf, pid5bf, hitopsr, hitopbr, hitophsum — adapt to HSUM's 3-card layout), deleting the now-duplicated inline blocks.
- [x] T6: Final verification — full `pkgdown::build_site()`, `devtools::test()`, confirm href lock passes and `inst/extdata/`/`data/` untouched; final light + dark screenshots of all 6 pages.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-16: created by /milestone-plan (promotes the 2026-07-16 candidate row; lineage M20/D-016).
- 2026-07-16: /milestone-implement — status in-progress, branch m21-beautify-download-pages cut from main.
- 2026-07-16: T1–T3 done — extra.css styles (`:has()` opt-in full-width, badges, collapsible), `_download-helpers.R` (raw-html fenced output; caught pandoc re-parsing `_1.0_` as emphasis), PID-5 prototype verified in browser light+dark; devtools::test() clean (9635 pass).
- 2026-07-16: checkpoint gate — iterate: MD5 column dropped from the versions table (audience-fit), Import Instructions demoted to a quiet text link; scope amendment approved (manifest wording cleanup); translations scaling punted to a ROADMAP candidate.
- 2026-07-17: iteration applied + manifest re-derived wording-only (rows verified identical minus `changes`); both themes re-verified in browser.
- 2026-07-17: T4 approved (design v2, gate); T5 rollout to the 5 remaining pages — all 6 render new components, zero inline styles; fixed pre-existing BF-page "PID-5-SF" typo; HSUM QSF card copy "text file" → "survey file"; tests clean (9635 pass).
- 2026-07-17: T6 done — full `pkgdown::build_site()` clean, extdata/data tree clean, 4 artifact hrefs per page; NEWS entry added; AC5 wording aligned to the gated manifest amendment; status → review.

## Decisions
<!-- owner: implement / review · append-only -->

## Review
<!-- owner: review · exclusive -->
