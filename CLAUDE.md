# hitop — R package for HiTOP Society questionnaire instruments

Scores, validity-screens, and distributes HiTOP Society instruments:
PID-5 (FULL 220 / SF 100 / BF 25, one function per task with a `version`
argument), HiTOP-SR (405), HiTOP-BR (45), and HiTOP-HSUM (item data +
export only; scoring pending Society feedback). Version 0.1.0, GPL-3.
Maintained by Jeffrey Girard.

## Commands

- Document: `Rscript -e 'devtools::document()'`
- Test: `Rscript -e 'devtools::test()'` (testthat edition 3; currently
  fails locally if Imports flextable/officer/snakecase are not
  installed)
- Check: `Rscript -e 'devtools::check()'`
- Coverage: `Rscript -e 'covr::package_coverage()'`
- Regenerate data: run the matching script in `data-raw/`; never edit
  `data/*.rda` or `R/sysdata.rda` directly
- README: edit `README.Rmd`, then
  `Rscript -e 'devtools::build_readme()'`
- Site: pkgdown, built/deployed by `.github/workflows/pkgdown.yaml`;
  local preview `Rscript -e 'pkgdown::build_site()'`

## Conventions

- Scoring signature:
  `(data, items, [version,] srange, prefix, missing, calc_se, append = TRUE)`;
  always returns a tibble. No `id`/`scales`/`tibble` arguments; all
  scales are computed. Reliability is a separate returning family
  `reliability_{pid5,hitopsr,hitopbr}(data, items, [version,] srange, alpha, omega)`
  → a per-scale tibble.
- Output columns: `prefix` + camelCase scale name (`pid_anhedonia`,
  `hsr_...`); validity scales are `prefix` + abbreviation (`pid_PNA`,
  `pid_INC`); standard errors add `_se`.
- Scale membership comes from the `*_scales` list-column tables
  (`pid_scales[[version]]$itemNumbers`, `hitopsr_scales`,
  `hitopbr_scales`); reverse-keying and validity-scale membership from
  the `*_items` tables. Never hardcode item numbers.
- User messaging via
  [`cli::cli_alert_warning()`](https://cli.r-lib.org/reference/cli_alert.html) +
  [`cli::cli_alert_info()`](https://cli.r-lib.org/reference/cli_alert.html)
  with actionable `{.code dplyr::filter(...)}` suggestions; validation
  via `validate_*()`/`cli_assert()` in `R/util.R`.
- Base-R data manipulation internally; {tibble} is an Import (default
  output), {lavaan} in Suggests (needed only by `calc_omega`).
- Instrument administration text lives in `R/sysdata.rda`
  (`*_instructions`); prebuilt DOCX/Qualtrics/REDCap artifacts in
  `inst/extdata/`.

## Hard rules

- Never hand-edit `NAMESPACE`, `man/`, `data/*.rda`, or `R/sysdata.rda`
  — they are generated (roxygen2, data-raw scripts).
- Never change keying content (`pid_items` and the other
  `*_items`/`*_scales` tables) without maintainer sign-off; provenance
  in `cairn/SOURCES.md`.
- New instruments follow the pattern: items (+ scales) table via
  `data-raw/` → `score_*` (+ `validity_*`) +
  `generate_{docx,qualtrics,redcap}_*` family (see cairn/DESIGN.md).
- Keep `tests/`, NEWS.md, and the `_pkgdown.yml` reference index in step
  with exported functions.

## Branching & PRs

- Milestone/code work happens on a `m<n>-<slug>` branch and merges via
  PR (record the URL in the milestone entry); keying-content PRs need
  Jeff’s explicit sign-off before merge. Direct commits to main only for
  trivial doc/tracking edits — main must stay installable (pak installs
  and pkgdown deploys from it).

## Working with Jeff

- When a primary source (paper, chapter, manual, scoring key) would
  help, look for it; if you can’t find or access it, ask Jeff to upload
  it. Never rely on secondary descriptions or memory for keying/scoring
  content.
- Assume the session usually runs on Opus: delegate tasks that suit a
  cheaper model to Sonnet subagents; if a task would benefit from a
  clean Fable session, ask Jeff to launch one.

## Project tracking (cairn)

This repo uses the cairn plugin. **Before acting on any request,
classify it and route** — the tracking rulebook only loads once a cairn
skill fires, so starting work in plain conversation silently bypasses
the work tiers and the git model. Classify first:

- **Trivial** (no runtime surface — typo, comment, tracking edit):
  commit directly to the default branch.
- **User-visible bug**: invoke `/hotfix`.
- **New work, a design decision, or more than one sitting**: invoke
  `/milestone-plan` (then `/milestone-implement` → `/milestone-review`).
- **Status, “what’s next”, or unsure which tier**: invoke `/milestone`.
- **Never implement code on the default branch** outside a
  milestone/hotfix branch; nothing reaches it without the user’s
  explicit approval at the review gate.

Whenever the request is anything but trivial, invoke the skill *first*
so the full rulebook (the plugin’s `skills/shared/tracking-rules.md`)
and its conduct load — do not reconstruct the rules here from memory.
All project state lives under `cairn/` (**Architecture → DESIGN · Status
→ ROADMAP · Tasks → milestone files · Decisions → DECISIONS · Lessons →
LESSONS · History → archive + git**); never record status or TODOs in
this file. Claude’s persistent memory never holds project state;
`cairn/` files win any conflict. Keying provenance (sources for
`pid_items` etc.) → the repo-specific file `cairn/SOURCES.md`.
