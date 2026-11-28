# Profile

## Verify

Run by `/milestone-implement` (per task) and `/hotfix` (gate-lite):
- After roxygen changes: `Rscript -e 'devtools::document()'`.
- After code changes, before a task is checked off: `Rscript -e 'devtools::test()'` clean.
- `/hotfix` gate-lite: `devtools::test()` clean; `devtools::document()` if
  roxygen changed; `devtools::check()` if anything structural was touched.

Toolchain checks `/milestone-review` runs *in addition to* the universal
cairn-file checks (`cairn_validate`, coverage completeness, `cairn_impact`):
- `devtools::document()` produces no diff.
- Generated files are never hand-edited: `NAMESPACE`, `man/`, and `data/*.rda`
  regenerate from roxygen and `data-raw/` scripts (the no-diff `document()`
  check catches drift).
- README.md is knitted from README.Rmd; present and out of sync with README.md → `devtools::build_readme()`, commit.
- pkgdown site present → `pkgdown::check_pkgdown()` passes (catches exports missing from `_pkgdown.yml`).
- NEWS.md has an entry for this milestone's user-visible changes (no milestone numbers in user-facing text).
- New top-level files have `.Rbuildignore` entries (check `check()` NOTEs).
- Full check at review: `Rscript -e 'devtools::check()'` clean (0 errors, 0 warnings; justify NOTEs).

## Changelog

The repo's changelog file, read by `/hotfix`, the release-walk, and the
consistency-gate: **`NEWS.md`** (the R-package convention).

## Release

Followed by `/cairn-release` — a CRAN release walk (never self-submits):
- Version decision (patch/minor/major) from NEWS.md; pre-1.0 conventions per DESIGN.md.
- NEWS consolidation: retitle the dev heading to the version; group entries; prune noise.
- Full local verification: `devtools::document()` (no diff), `devtools::test()`
  and `devtools::check()` clean, `devtools::build_readme()`, `pkgdown::check_pkgdown()`,
  `urlchecker::url_check()`.
- Wide checks as applicable: `devtools::check_win_devel()` and/or R-hub; `revdepcheck` if dependents exist.
- Update `cran-comments.md` (test environments, check results, NOTE justifications, revdep summary).
- Bump `Version:` in DESCRIPTION.
- Handoff checklist (user runs): `devtools::submit_cran()`, confirm the CRAN
  email, then `usethis::use_github_release()` + `usethis::use_dev_version()`.
- Downstream: the browser builder [`jmgirard/hitop-builder`](https://github.com/jmgirard/hitop-builder)
  installs this package from r-universe at page load and declares the oldest
  version it will build against as `MIN_HITOP` in its `index.html`. If the
  release changes any surface that page calls — `available_scales()`,
  `hitop_module()`, `write_module()`, `scale_definitions()`, or the three
  `generate_*_hitopsr()` functions with `descriptor` — raise `MIN_HITOP` to this
  version and update the minimum stated in that repo's `README.md`, in a pull
  request of its own. A release touching none of those surfaces leaves both
  alone.
- Downstream watch: that repo's `tests/smoke.spec.js` boots the page headlessly
  and downloads a Word form, run by `.github/workflows/smoke.yml` on four
  triggers — `pull_request` and `push` to `main` against its own checkout, a
  weekly `schedule` and `workflow_dispatch` against the deployed page. The
  weekly run notices a release breaking the deployed page; after a release
  raising `MIN_HITOP`, dispatch it rather than wait for the schedule.
