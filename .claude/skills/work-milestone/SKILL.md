---
name: work-milestone
description: Implement a planned milestone for the hitop package. Use when the user says to work on, implement, start, continue, or resume a milestone (e.g. "work on M3"), or to implement something already planned in project/MILESTONES.md.
---

# Work a milestone

Implement a READY milestone, keeping MILESTONES.md true to reality at every step.

## Steps

1. Read `.claude/skills/shared/tracking-rules.md` and `project/MILESTONES.md`; resolve the target milestone (named id, or the single IN PROGRESS one when resuming).

2. **Gate checks:**
   - Status must be `READY` or `IN PROGRESS`. If `PLANNED`, stop and suggest `/plan-milestone M<n>` first. If `DONE`/`DROPPED`, stop and ask.
   - If any `Depends on` milestone is not DONE, warn the user and get explicit confirmation before proceeding.
   - If a *different* milestone is IN PROGRESS, warn (one-at-a-time rule) and ask.

3. Read the Conventions and relevant Architecture sections of `project/DESIGN.md`, plus every file referenced in the milestone's Tasks.

4. **Create the milestone branch before touching any code:** `m<n>-<slug>` (e.g. `m1-keying-tests`) cut from up-to-date main, per the Branching & PRs rules in tracking-rules.md. If resuming, check out the existing branch. Then **set status → `IN PROGRESS` in MILESTONES.md**, so an interrupted session still leaves accurate state.

5. **Work through Tasks in order.** After completing each task, immediately check its box (`- [x]`) in MILESTONES.md — incremental updates, never one batch at the end. Add discovered subtasks as new unchecked boxes rather than doing untracked work. Follow the R guardrails in tracking-rules.md (document/test after changes; never hand-edit generated files).

6. **If blocked:** set status → `BLOCKED`, add a `**Blocked by:**` line to the entry, append a LOG.md entry describing the blocker, and report to the user.

7. **When all tasks are checked:** do NOT mark DONE and do not check acceptance-criteria boxes — that is `/review-milestone`'s job. Append a LOG.md session entry (what changed, decisions, follow-ups). If implementation deviated from plan in an architecturally meaningful way, append a D-entry to DESIGN.md and update the affected section.

8. **Open the PR:** commit the work to the milestone branch, push it, and open a PR (`gh pr create`) whose body summarizes the milestone Goal and tasks; record the PR URL in the milestone's **Notes/links**. Then tell the user implementation is complete and to run `/review-milestone M<n>` to verify and close it.
