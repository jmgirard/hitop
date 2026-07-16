---
name: plan-milestone
description: Plan a milestone for the hitop package. Use when the user wants to plan, scope, break down, or add a milestone or feature to project/MILESTONES.md, or asks "what should we work on next".
---

# Plan a milestone

Turn an idea (or an existing PLANNED entry) into a READY milestone with verifiable acceptance criteria and concrete tasks.

## Steps

1. Read `.claude/skills/shared/tracking-rules.md`, `project/MILESTONES.md`, and `project/ROADMAP.md`. If the work touches architecture (new function family, new instrument, dependency changes), also read `project/DESIGN.md`.

2. **Resolve the target:**
   - If the user named a milestone (e.g., "plan M4"), use that entry.
   - If the user described new work, create a new entry using the next unused `M<n>` id (check both Active and Completed sections — ids are never reused).
   - If the user asked "what next": recommend the highest-priority milestone whose dependencies are all DONE, respecting the roadmap's current phase. Present the recommendation and confirm before planning it.

3. **Explore the relevant code before drafting.** Read the actual files the milestone touches; cite `file:line` in the Tasks. Look for existing helpers/patterns to reuse (R/util.R, the keying-table pattern) rather than proposing new machinery. For milestones spanning more than a couple of files, delegate the fan-out to `Explore` subagents per the Subagent-use rules in tracking-rules.md (default/inherited model — never Haiku).

3b. **Plan the oracle before planning the tasks** for any milestone that computes or changes scores: decide which of the oracle-strategy checks (tracking-rules.md) apply, and write them into the Tasks and Acceptance criteria explicitly (hand-computed fixture values, independent recomputation, invariants).

4. **Draft the entry** using the template in tracking-rules.md:
   - Goal: one sentence.
   - Acceptance criteria: each objectively checkable by running a command or inspecting a file. Code milestones always include "`devtools::check()` clean" and, once tests exist, "`devtools::test()` passes".
   - Tasks: ordered, concrete, with file references. Test-first where a bug is involved.
   - Depends on: other milestone ids, or —.

5. **Present the draft to the user and get approval before writing anything.**

6. **Write:**
   - The approved entry in `project/MILESTONES.md`, status → `READY`.
   - If planning surfaced an architectural decision, append a `D-<next>` entry to the DESIGN.md Decision Log.
   - If it revealed a new long-term theme, add a bullet to ROADMAP.md's Someday-maybe (or the relevant phase).
   - Do NOT write a LOG.md entry — planning is not shipped work.

7. Tell the user the milestone is READY and that `/work-milestone M<n>` starts implementation.
