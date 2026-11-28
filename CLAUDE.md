# hitop — R package for HiTOP Society questionnaire instruments

Scores, validity-screens, and distributes HiTOP Society instruments: PID-5 (FULL 220 / SF 100 / BF 25 / BFPM 36 / IRF 218 / FFBF 100, one function per task with a `version` argument), HiTOP-SR (405), HiTOP-BR (45), and HiTOP-HSUM (item data + export only; scoring pending Society feedback). Version 0.2.0, GPL-3. Maintained by Jeffrey Girard.

## Commands

- Document: `Rscript -e 'devtools::document()'`
- Test: `Rscript -e 'devtools::test()'` (testthat edition 3; currently fails locally if Imports flextable/officer/snakecase are not installed)
- Check: `Rscript -e 'devtools::check()'`
- Coverage: `Rscript -e 'covr::package_coverage()'`
- Regenerate data: run the matching script in `data-raw/`; never edit `data/*.rda` or `R/sysdata.rda` directly
- README: edit `README.Rmd`, then `Rscript -e 'devtools::build_readme()'`
- Line endings: LF everywhere via `.gitattributes`; `Rscript data-raw/check_line_endings.R` verifies it and runs in CI. `git config blame.ignoreRevsFile .git-blame-ignore-revs` makes `git blame` skip whatever commits that file lists (it is per-clone; a fresh clone does not inherit it).
- Site: pkgdown, built/deployed by `.github/workflows/pkgdown.yaml`; local preview `Rscript -e 'pkgdown::build_site()'`

## Conventions

- Scoring signature: `(data, items, [version,] srange, prefix, missing, calc_se, append = TRUE)`; always returns a tibble. `score_hitopdat(data, items, prefix, min_complete, append = TRUE)` departs from it (D-099). No `id`/`scales`/`tibble` arguments; all scales are computed. Reliability is a separate returning family `reliability_{pid5,hitopsr,hitopbr}(data, items, [version,] srange, alpha, omega)` → a per-scale tibble.
- Output columns: `prefix` + camelCase scale name (`pid_anhedonia`, `hsr_...`); validity scales are `prefix` + abbreviation (`pid_PNA`, `pid_INC`); standard errors add `_se`.
- Scale membership comes from the `*_scales` list-column tables (`pid_scales[[version]]$itemNumbers`, `hitopsr_scales`, `hitopbr_scales`); reverse-keying and validity-scale membership from the `*_items` tables. Never hardcode item numbers.
- User messaging via `cli::cli_alert_warning()` + `cli::cli_alert_info()` with actionable `{.code dplyr::filter(...)}` suggestions — except reports the caller is meant to catch or suppress, which are `cli::cli_warn()` conditions (D-024/D-025 carve-out, see DESIGN "User communication"); validation via `validate_*()`/`cli_assert()` in `R/util.R`.
- Base-R data manipulation internally; {tibble} is an Import (default output), {lavaan} in Suggests (needed only by `calc_omega`).
- Instrument administration text lives in `R/sysdata.rda` (`*_instructions`); prebuilt DOCX/Qualtrics/REDCap artifacts in `inst/extdata/`.

## Hard rules

- Never hand-edit `NAMESPACE`, `man/`, `data/*.rda`, or `R/sysdata.rda` — they are generated (roxygen2, data-raw scripts).
- Never change keying content (`pid_items` and the other `*_items`/`*_scales` tables) without maintainer sign-off; provenance in `cairn/SOURCES.md`.
- New instruments follow the pattern: items (+ scales) table via `data-raw/` → `score_*` (+ `validity_*`) + `generate_{docx,qualtrics,redcap}_*` family (see cairn/DESIGN.md).
- Keep `tests/`, NEWS.md, and the `_pkgdown.yml` reference index in step with exported functions, and the README capability table ("What the package covers", checked by `test-readme-capabilities.R`) in step with the functions, the scoring tutorials and `hitop_artifacts`.

## Branching & PRs

- Milestone/code work happens on a `m<n>-<slug>` branch and merges via PR (record the URL in the milestone entry); keying-content PRs need Jeff's explicit sign-off before merge. Direct commits to main only for trivial doc/tracking edits — main must stay installable (pak installs and pkgdown deploys from it).

## Working with Jeff

- When a primary source (paper, chapter, manual, scoring key) would help, look for it; if you can't find or access it, ask Jeff to upload it. Never rely on secondary descriptions or memory for keying/scoring content.
- Assume the session usually runs on Opus: delegate tasks that suit a cheaper model to Sonnet subagents; if a task would benefit from a clean Fable session, ask Jeff to launch one.

## Project tracking (cairn)

This repo uses the cairn plugin. Work goes through its `/cairn` skill:

- **Any request**: `/cairn start`. It handles a trivial edit itself (no
  runtime surface: a typo, a comment, a ROADMAP row), and it plans anything
  else on a branch and takes it to review.
- **Work in flight**: `/cairn resume` picks it up, and `/cairn review` ships it.
- **Status or "what's next"**: `/cairn status`.

Never build on the default branch. Nothing reaches it without the user's
approval at the merge question, or for a trivial edit their request.

Candidates are in `cairn/ROADMAP.md`, and the toolchain commands are in
`cairn/PROFILE.md`. Plans live outside the repo, in the home folder that
`/cairn status` prints. Never record status or TODOs in this file. Keying
provenance (sources for `pid_items` etc.) is in the repo-specific file
`cairn/SOURCES.md`.
