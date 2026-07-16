---
name: sync-docs
description: Audit and update hitop's project tracking docs (CLAUDE.md, project/DESIGN.md, ROADMAP.md, MILESTONES.md, LOG.md) so they match the actual state of the code. Use when docs may be stale, after manual changes made without skills, or when the user asks to update, sync, or refresh project docs.
---

# Sync tracking docs with reality

Reconcile the five tracking files against the actual repo state. Ground truth is the code and git history, never the docs.

## Steps

1. Read `.claude/skills/shared/tracking-rules.md` and all five tracking files.

2. **Gather ground truth:**
   - `git log --oneline -15` and `git status`
   - Contents of `R/`, `tests/` (if present), `.github/workflows/` (if present), `vignettes/` (if present)
   - `DESCRIPTION` and `NAMESPACE` (current exports/deps)

3. **Cross-check each doc:**
   - **MILESTONES.md:** For each IN PROGRESS/READY milestone, spot-check whether its acceptance criteria now pass (it may be effectively done). For each DONE claim in Completed, confirm reality doesn't contradict it. Flag PLANNED milestones whose dependencies are now all DONE as plannable. Verify at most one IN PROGRESS.
   - **DESIGN.md:** Exports, datasets, and conventions still accurate? For each Known issue, grep the code to confirm it still exists — remove fixed ones. Decision Log intact (append-only, no renumbering)?
   - **ROADMAP.md:** Is the "(current)" phase still correct given completed milestones?
   - **CLAUDE.md:** Every listed command actually runnable? Pointers valid? Still under ~60 lines — if it grew, propose moving content to DESIGN.md.
   - **LOG.md:** Do recent commits appear in some entry? If there's a gap, draft ONE catch-up entry summarizing the gap from `git log` — never fabricate per-day history.

4. **Present the discrepancy list to the user** (grouped by file, with evidence) and get approval before editing.

5. **Apply approved fixes**, respecting ownership boundaries (move misplaced content to its owning file rather than duplicating).

6. Append a LOG.md entry: `docs sync — fixed X, Y; flagged Z`.
