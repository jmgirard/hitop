---
name: review-milestone
description: Review and close out a milestone for the hitop package. Use when the user asks to review, verify, finish, complete, or close a milestone, or after implementation work is done and needs checking.
---

# Review a milestone

Verify a milestone's acceptance criteria by executing them, then either close it out (with doc sync) or send it back.

## Steps

1. Read `.claude/skills/shared/tracking-rules.md` and the milestone's entry in `project/MILESTONES.md` (named id, or the IN PROGRESS one). Check out the milestone's branch and locate its PR (from Notes/links, or `gh pr list`); the PR diff is what you are reviewing. If there is no branch/PR, flag the process gap and review the working diff instead.

2. **Verify every acceptance criterion by executing it** — run the command, confirm the file exists, re-read the changed code. Never trust existing checkboxes or the session's memory. Check each box only after its check actually passes.

3. **Run the standard gate** regardless of the criteria:
   - `Rscript -e 'devtools::document()'` — must produce no diff (`git diff --stat NAMESPACE man/`)
   - `Rscript -e 'devtools::test()'` (once tests exist)
   - `Rscript -e 'devtools::check()'` — 0 errors, 0 warnings
   Report actual results; do not summarize failures away.

4. **Fresh-context diff review via subagent.** Launch a `general-purpose` subagent (default/inherited model — never Haiku) that has not seen the implementation work. Give it: the milestone's Goal + acceptance criteria, the diff (`git diff` scope for the work), and pointers to DESIGN.md Conventions and the Oracle-strategy section of tracking-rules.md. Ask it to verify (a) the diff actually satisfies each criterion, (b) conventions are followed (signature pattern, naming prefixes, cli messaging, base-R internals), and (c) for scoring changes, that tests verify against ground truth (hand-computed fixtures / independent recomputation), not merely against the code's own output. Treat its findings as review findings — verify each before acting, then flag confirmed ones.

5. **If anything fails:** leave status `IN PROGRESS`, uncheck the failed criteria boxes, record findings in the entry's Notes, and recommend the next action (usually `/work-milestone M<n>` to address findings).

6. **If everything passes — get merge authorization, then close out:**
   - **Keying-content gate first.** If the PR touches keying content (`pid_items` or other `*_items`/`*_scales` tables), stop and get Jeff's explicit sign-off in the PR before going any further; do not present the merge prompt until he has signed off.
   - **Ask before merging (always).** Present a yes/no prompt via `AskUserQuestion` — header `Merge PR #<n>`, options **"Yes — merge it for me"** and **"No — I'll merge / hold"** (add the one-line verification verdict in the question so Jeff can decide). A "Yes" is Jeff's explicit authorization for you to run `gh pr merge <n> --merge`; the harness blocks the agent from merging a self-authored PR without it. If the harness still denies the merge after a "Yes", tell Jeff and let him merge it himself or add a Bash permission rule — do not attempt to work around the denial. If "No", leave the milestone `IN PROGRESS` with the passing review recorded in Notes and stop here.
   - **Only after the PR is actually merged** (by you or by Jeff), on up-to-date `main`: check the acceptance boxes; set Status → `DONE`; move the entry to Completed, collapsed per tracking-rules.md, keeping the checked criteria.
   - Append a LOG.md entry (what shipped, verification results, follow-ups).
   - **Inline doc sync:** remove now-fixed items from DESIGN.md Known issues; update the affected DESIGN.md sections if the architecture changed; update ROADMAP.md if this completed a phase outcome (and advance the "(current)" phase marker if the phase is finished); update CLAUDE.md Commands/Conventions if tooling changed (e.g., remove the "activates at M1" caveat once tests exist).

7. Suggest the next milestone: the highest-priority READY entry whose dependencies are DONE, or `/plan-milestone` if none are READY.
