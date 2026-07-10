# Work Log

<!-- Append-only, newest first. One entry per work session or merged change. -->
<!-- Format: ## YYYY-MM-DD — Title (M-refs) / What / Why-decisions / Follow-ups. Never fabricate history. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- This log starts fresh for the canonical repo (v0.1.0); the old fork's log was deliberately not imported (D-006). Pre-tracking history: see git log and NEWS.md. -->

## 2026-07-09 — Adopted branch + PR workflow (—)

- **What:** Added "Branching & PRs" rules to CLAUDE.md and tracking-rules.md: milestone work on `m<n>-<slug>` branches, merged to main via PR with the URL recorded in the milestone entry; keying-content PRs require Jeff's explicit sign-off before merge; trivial doc/tracking edits may still go direct to main. Updated /work-milestone (branch before code, open PR at completion) and /review-milestone (review the PR diff, merge on pass) to match.
- **Why / decisions:** main is a distribution channel (pak installs and pkgdown deploys from it) and must stay installable; PRs give CI (future M4) a gate and keying changes a sign-off point.
- **Follow-ups spawned:** —

## 2026-07-09 — Added collaboration rules: primary sources & model choice (—)

- **What:** Added two working rules to CLAUDE.md ("Working with Jeff") and `.claude/skills/shared/tracking-rules.md`: (1) seek primary sources when they would help and ask Jeff to upload any that can't be found/accessed — never substitute secondary descriptions or model memory for keying/scoring content; (2) assume the main session runs on Opus, delegate suitable tasks to Sonnet subagents, and ask Jeff to launch a clean Fable session when one would help.
- **Why / decisions:** Maintainer request; extends the existing subagent model-choice rule (no Haiku for accuracy-critical work).
- **Follow-ups spawned:** —

## 2026-07-09 — Tracking docs bootstrapped from code (M1–M5)

- **What:** The tracking system (CLAUDE.md, project/, .claude skills) was copied in from an older PID-5-only fork; its docs described that fork, not this package. Established ground truth from DESCRIPTION, NAMESPACE, R/, tests/, data/, .github/, vignettes/, and _pkgdown.yml, then rewrote CLAUDE.md, DESIGN.md, ROADMAP.md, and MILESTONES.md to match reality (v0.1.0; PID-5 FULL/SF/BF via `version` arg; HiTOP-SR/BR/HSUM; reliability, label/rename/rank, and generator families; pkgdown + vignettes shipped). SOURCES.md kept (keying provenance re-verified against this repo's data-raw/pid_items.csv); D-001–D-005 preserved as history; D-006 added.
- **Verified while auditing:** the old fork's SDTD guard bug, commented-out SF warnings, placeholder DESCRIPTION, and missing BF support do **not** exist here. Still real: no PID-5/keying/reliability tests, no check/coverage CI. Newly found: 6 undocumented data/*.rda (instructions duplicated in sysdata), {httr2} used undeclared while {glue}/{lifecycle}/{jsonlite} are unused Imports, redundant `utils::data()` calls (work, but pollute the global env), and local test/check blocked by uninstalled Imports (flextable/officer/snakecase). Recorded as DESIGN Known issues #1–#8.
- **Why / decisions:** D-006 (re-baseline on the canonical repo; port — don't copy — the fork's oracle tests).
- **Follow-ups spawned:** M1 (keying tests), M2 (PID-5 oracle tests), M3 (check/dependency hygiene), M4 (CI + coverage), M5 (HiTOP oracle tests). OQ-1 (SDTD item 38) remains open pending the maintainer's check of the physical PID-5 manual.
