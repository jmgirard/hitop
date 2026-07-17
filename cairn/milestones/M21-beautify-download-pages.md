<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M21: Beautify instrument download pages

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate -->
- **Depends on:** —   <!-- M20 shipped; lineage only -->
- **Principles touched:** GP2   <!-- works under: version info stays manifest-driven and user-visible (D-016) -->
- **Branch/PR:** m21-beautify-download-pages · https://github.com/jmgirard/hitop/pull/23   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create -->

Redesign the six pkgdown instrument download pages so build-date version info is visible at the point of download and the M20 manifest tables become a polished, theme-aware, collapsible versions section.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**
- All 6 `vignettes/articles/download-*.Rmd` pages: whole-page visual polish (spacing, card refinement, section headers) plus the Versions-section redesign.
- Version treatment (gate-approved 2026-07-16): build-date badges on/near each download button; current-builds manifest (file, format, instrument version, build date — MD5 dropped from the table at the 2026-07-16 checkpoint gate for audience-fit; checksums remain in `hitop_artifacts`) and version history rendered as custom HTML from `hitop_artifacts` inside a styled collapsible (`<details>`) section.
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

- [x] AC1: Each of the 6 download pages shows a build-date indicator at/near each download button, sourced from `hitop_artifacts` at render time — no hardcoded dates in page source (grep evidence + rendered-page check).
- [x] AC2: Each page's Versions section renders the current-builds manifest (file, format, instrument version, build date — MD5 column dropped per the 2026-07-16 checkpoint-gate iteration; checksums remain available via `hitop_artifacts`) and full version history from `hitop_artifacts` inside a styled collapsible element (rendered-page evidence).
- [x] AC3: No duplicated inline `<style>` blocks remain in `download-*.Rmd`; shared CSS lives in `pkgdown/extra.css` and version rendering in exactly one shared helper file (grep evidence).
- [x] AC4: Pages render correctly in both light and dark themes (screenshots from locally built site, both themes).
- [x] AC5: `devtools::test()` clean — in particular the download-page href lock in `tests/testthat/test-artifacts.R` — with no `inst/extdata/` changes; `data/hitop_artifacts.rda` changed only per the gated wording-only amendment (dates and MD5s identical).
- [x] AC6: Prototype checkpoint honored: work-log line records Jeff's approval of the PID-5 page design before rollout commits to the other 5 pages.
- [x] AC7: Local `pkgdown::build_site()` completes without errors and all 6 pages render.

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
- 2026-07-17: review — AC2 wording aligned to the recorded 2026-07-16 checkpoint-gate iteration (MD5 column dropped); amendment applied per the gate decision already in this work log, not a review-side reinterpretation.

## Decisions
<!-- owner: implement / review · append-only -->

## Review
<!-- owner: review · exclusive -->

**PR:** https://github.com/jmgirard/hitop/pull/23 (draft). Reviewed 2026-07-17.

### Acceptance-criteria evidence

- AC1 (build-date indicator per button, no hardcoded dates) — PASS. `grep` for `YYYY-MM-DD` in the 6 `download-*.Rmd` sources: 0 matches. Rendered pages: 9–10 `hitop-build-badge` spans each. Dates come from `hitop_artifacts` at render (`download_cards()` looks up `current$build_date` by file basename).
- AC2 (collapsible Versions panel w/ current builds + full history from manifest) — PASS. Each rendered page has exactly one `<details class="hitop-versions">`; HSUM body text confirmed to list all 4 current builds + both history entries from the manifest.
- AC3 (no duplicated inline `<style>`; CSS in extra.css; one shared helper) — PASS. `grep '<style'` over the 6 sources: 0 matches. One helper: `vignettes/articles/_download-helpers.R`.
- AC4 (both themes) — PASS. Computed styles probed live: light `details` bg rgb(255,255,255)/border rgb(222,226,230), badge bg rgb(233,236,239)/text rgba(33,37,41,.75); dark `details` bg rgb(33,37,41)/border rgb(73,80,87), badge bg rgb(52,58,64)/text rgba(222,226,230,.75). All from Bootstrap theme variables; both themes distinct and legible. Screenshots captured light+dark.
- AC5 (tests clean incl. href lock; extdata unchanged; data changed only per gated wording amendment) — PASS. `devtools::test()` 9635 pass / 0 fail (href-lock test `download-page links point at committed artifacts` included). `git diff --name-only main..HEAD -- inst/extdata`: empty. `-- data`: only `data/hitop_artifacts.rda` (gated wording-only; dates/MD5s verified identical earlier).
- AC6 (prototype checkpoint approved before rollout) — PASS. Work-log 2026-07-17: "T4 approved (design v2, gate)" precedes the T5 rollout commit (698d3bb).
- AC7 (`pkgdown::build_site()` clean, all 6 pages) — PASS. Full `build_site()` finished with no errors; 6 `download-*.html` present with new components.

### Consistency gate

- `cairn_validate.py` — exit 0, all checks passed (20 advisory dangling-id warnings, all pre-existing legacy D-001..D-012/M13/M14 refs; not gate failures).
- No IPn/GPn principle changed (M21 works under GP2) → `cairn_impact` skipped.
- Coverage map: all 7 ACs map to existing tasks — complete.
- Toolchain (`r-package` consistency-gate): `devtools::document()` no diff; `pkgdown::check_pkgdown()` "No problems found"; NEWS.md has the M21 entry; `devtools::check()` **0 errors / 0 warnings / 0 notes**.
- CI (PR #23): 7/7 green — ubuntu devel/release/oldrel-1, macOS, Windows R CMD check all pass; pkgdown + test-coverage pass.

### Independent review (3 lenses + scorer)

- **[O] diff-bug (Opus):** one finding — the Scope "In" line still said "(incl. MD5)" after AC2 had been reconciled, an internal contradiction with the shipped MD5-less table. Helper R code, CSS (both themes), content fidelity (all 6 pages keep their 4 artifact links + Import Instructions ref), and the wording-only `.rda` change all verified clean; ran the artifact suite (47/47).
- **[S] blame-history (Sonnet):** no findings. Confirmed the MD5-table drop does not undo D-016 (the manifest + md5-lock test are untouched, 47/47), the `.rda` change is byte-identical except `changes` wording, `inst/extdata/` untouched (stable-URL guarantee), and the `.gitignore` negation exposes no stray files.
- **[S] prior-PR-comments (Sonnet):** no prior-PR evidence — PRs #17 and #22 (the merged PRs touching these files) carry zero review comments; clean no-op.
- **[S] scorer (Sonnet):** scored the single finding **60** (< 80 threshold → logged, not actioned as a code finding): the finding was half-stale (AC2 already reconciled) but flagged a real residual — the Scope "(incl. MD5)" line. Residual aligned as a documentary edit backed by the already-recorded 2026-07-16 gate decision (Scope In bullet now names the MD5-drop). No code findings actioned; no follow-ups spawned.
