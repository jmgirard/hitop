<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". -->
# M20: Artifact versioning — build-date manifest + checksum lock

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** IP1, GP2, GP4
- **Branch/PR:** m20-artifact-versioning

## Goal

Give every distributed `inst/extdata/` artifact a user-visible build-date version — a checksum-locked manifest dataset, embedded DOCX/QSF stamps, and download-page version history — so an artifact can never again change at a stable URL without a visible version signal (the M19 QSF gap).

## Scope

**In:**

- New exported manifest dataset `hitop_artifacts` (one row per file per build: `file`, `instrument`, `format`, `instrument_version`, `build_date`, `md5`, `changes`), built by a new `data-raw/artifacts.R` that also regenerates all committed artifacts (except the QSF, which is API-built by `devel/qualtrics_hitophsum.R` and manifest-rowed from disk).
- Build stamp in the DOCX footer ("Generated YYYY-MM-DD · hitop X.Y.Z", beside the existing copyright line); instrument version stays in the page-header title ("(v1.0)"). No generator signature changes.
- Filenames drop the instrument version (`pid5_1.0_A4.docx` → `pid5_A4.docx`); generator default `file` arguments updated to match; old `_1.0_` raw/main URLs break once (accepted, Jeff 2026-07-16).
- Lock test: bidirectional file↔manifest completeness + `tools::md5sum()` match, plus a download-page href check against the committed files.
- All 6 `vignettes/articles/download-*.Rmd` pages: updated links, current-build-date label, per-instrument version-history table rendered from `hitop_artifacts`.
- NEWS entry; DESIGN.md conventions note for the versioning scheme.

**Out:**

- QSF rebuild — the committed QSF already carries its stamp (`SurveyName` "…rebuilt 2026-07-16") and is content-locked by test (M19); the manifest records it as-is.
- Embedded stamps in the flat Qualtrics `.txt` and REDCap zips — the formats have no safe metadata slot; the manifest and download pages cover them (D-016).
- sha256/cryptographic hashing — md5 change-detection only, zero new deps (D-016).
- Any change to item text, response options, or instructions — regeneration must be content-identical (IP1); content work has its own milestones.

## Acceptance criteria

- [ ] AC1: `hitop_artifacts` is exported, documented in `R/data.R`, and listed in `_pkgdown.yml`; it holds one current row per committed `inst/extdata/` file with the seven columns above, plus the QSF's 2026-07-16 history row.
- [ ] AC2: A test fails when any committed artifact changes without a manifest update, in both directions (file without current row; current row whose md5 mismatches or whose file is absent) — demonstrated by mutation (touch a byte → test fails).
- [ ] AC3: Every committed DOCX artifact's footer carries the build stamp, and a parse-back test asserts the stamp's date equals that file's manifest `build_date`.
- [ ] AC4: No `inst/extdata/` filename contains an instrument version; generator default `file` values match the committed naming scheme; a test verifies every `download-*.Rmd` artifact href names a committed file.
- [ ] AC5: Each of the 6 download pages displays the current build date per artifact and a version-history table rendered from `hitop_artifacts` (local `pkgdown::build_site()` evidence).
- [ ] AC6: The existing parse-and-compare generator tests pass unchanged against the regenerated artifacts (content-identity under IP1 — the stamp is the only visible addition).
- [ ] AC7: Profile verify clean (`devtools::document()` + `devtools::test()`; `devtools::check()` before review).

## Coverage

- AC1 → T1, T4
- AC2 → T5
- AC3 → T2, T3, T5
- AC4 → T3, T5
- AC5 → T6
- AC6 → T3, T5
- AC7 → T7

## Tasks

- [x] T1: Write `data-raw/artifacts.R` — regenerates all committed artifacts (12 DOCX, 5 Qualtrics .txt, 6 REDCap zips) under the new names, computes `tools::md5sum()` per file, and writes `hitop_artifacts` via `usethis::use_data()` (QSF: manifest row from the file on disk, build_date 2026-07-16, no rebuild).
- [x] T2: Add the build stamp to both DOCX footer sites (now a shared `build_docx_footer()` helper in [R/generate_docx.R](../../R/generate_docx.R)) — "Generated YYYY-MM-DD · hitop X.Y.Z" via `Sys.Date()` + `utils::packageVersion("hitop")`; no signature changes.
- [x] T3: Rename: update generator default `file` args (drop `_1.0`), run `data-raw/artifacts.R`, `git rm` the old filenames, commit regenerated artifacts + manifest.
- [x] T4: Document `hitop_artifacts` in `R/data.R`; add to `_pkgdown.yml` reference index; `devtools::document()`.
- [x] T5: Write `tests/testthat/test-artifacts.R` — bidirectional manifest↔file lock (md5 + completeness), DOCX footer-stamp parse-back vs manifest, download-page href check; extend docx parse helpers only if the footer part needs a reader.
- [x] T6: Update the 6 `vignettes/articles/download-*.Rmd` — new hrefs, current-build-date labels, version-history table from `hitop_artifacts`; verify with local `pkgdown::build_site()`.
- [ ] T7: NEWS entry (renames + versioning system); DESIGN.md conventions note; full `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (question gate: build-date revisions, embed+manifest, version-free filenames, checksum lock → D-016).
- 2026-07-16: T1–T3 done — footer stamp via shared `build_docx_footer()`, 23 artifacts rebuilt under version-free names, 24-row manifest; IP1 identity verified (DOCX body XML byte-identical, .txt byte-identical, zip contents identical vs HEAD); tests 9588 pass / 0 fail.
- 2026-07-16: T4–T5 done — `hitop_artifacts` documented + pkgdown-listed; `test-artifacts.R` lock suite added (40 pass; href test red on the download pages pending T6, as intended).
- 2026-07-16: minor amendment — the plan miscounted the download pages: there are 6 (one per instrument form), not 7; AC5/T6/Scope wording corrected, page set unchanged (all of them).
- 2026-07-16: T6 done — 6 download pages: version-free hrefs, Versions section (current-builds + history tables from `hitop_artifacts`); rendered check via `pkgdown::build_article()` (tables + build dates + md5 present, no stale links); artifact test suite 47 pass / 0 fail.

## Decisions

## Review
