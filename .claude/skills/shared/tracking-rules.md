# Tracking rules (shared by all hitop project skills)

Read this before touching any tracking file. These rules keep the five files from drifting or overlapping.

## Ownership boundaries

| File | Owns | Does NOT own |
|---|---|---|
| `CLAUDE.md` | Commands, conventions, hard rules, pointers. Keep under ~60 lines. | Architecture rationale, task lists, history |
| `project/DESIGN.md` | Architecture as it IS; Known issues & tech debt; Decision Log | Future work, task status |
| `project/ROADMAP.md` | Phases, outcome end-states, someday-maybe list | Individual tasks, acceptance criteria, dates |
| `project/MILESTONES.md` | Milestone entries: status, deps, acceptance criteria, tasks | Vision statements, architecture, narrative history |
| `project/LOG.md` | Append-only dated journal (newest first) | Anything forward-looking |

Boundary rule: **Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG.** If content fits two files, put the substance in the owner and a one-line cross-reference in the other.

## Milestone status vocabulary and transitions

Exactly these six values: `PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED`

- `PLANNED` — idea captured; Goal filled in; Tasks/criteria may be incomplete.
- `READY` — fully planned: Goal, objectively verifiable Acceptance criteria, ordered Tasks, Depends on. Set only by /plan-milestone (or a human).
- `IN PROGRESS` — being worked. At most ONE milestone at a time (soft rule; warn if violated). Set by /work-milestone before any code is touched.
- `BLOCKED` — add a `**Blocked by:**` line and a LOG.md entry explaining the blocker.
- `DONE` — set only by /review-milestone after executing every acceptance criterion. Requires all acceptance boxes checked. Move the entry to the Completed section, collapsed to: `### M<n>: Title — DONE YYYY-MM-DD. One-line outcome.`
- `DROPPED` — deliberately abandoned; keep a one-liner in Completed with the reason.

IDs are `M<n>`, monotonically increasing, never reused — even for dropped milestones.

## Milestone entry template

```markdown
### M<n>: Title

- **Status:** PLANNED
- **Depends on:** M<x>, M<y> (or —)
- **Goal:** One sentence.
- **Acceptance criteria:**
  - [ ] Each criterion objectively checkable (a command that passes, a file that exists).
  - [ ] Code milestones always include: `devtools::check()` clean
- **Tasks:**
  - [ ] Ordered concrete steps with file:line references where known
- **Notes/links:** file refs, D-entry refs, issue/PR URLs
```

## Decision Log entry template (append to DESIGN.md, never renumber)

```markdown
### D-00n (YYYY-MM-DD): Title

**Context:** 1–2 lines.
**Decision:** 1–2 lines.
**Consequences:** 1–2 lines.
```

## LOG.md entry template (prepend below the header comments — newest first)

```markdown
## YYYY-MM-DD — Title (M-refs)

- **What:** bullets of changes.
- **Why / decisions:** link D-xxx if any.
- **Follow-ups spawned:** M-ids or —.
```

Never fabricate history. If LOG.md has a gap, add one catch-up entry summarizing `git log` output.

## Branching & PRs

`main` is a distribution channel, not just a dev branch: users install it directly (`pak::pak("jmgirard/hitop")`) and pkgdown deploys the site from it, so it must stay installable at all times.

- Milestone work happens on a branch named `m<n>-<slug>` (e.g. `m1-keying-tests`), cut from up-to-date main before any code is touched. Never commit milestone work directly to main.
- When implementation is ready, push the branch and open a PR (`gh pr create`); record the PR URL in the milestone's **Notes/links**. The PR is what /review-milestone reviews, and it merges before the milestone is marked DONE.
- Any change to keying content (`pid_items` or the other `*_items`/`*_scales` tables) always goes through a PR and requires Jeff's explicit sign-off in that PR before merge — never self-merge those.
- Trivial edits with no runtime surface (typos, tracking-doc updates like LOG entries) may commit directly to main.

## Subagent use

- **Exploration** (plan-milestone): for milestones touching more than a couple of files, delegate fan-out searching to `Explore` subagents and keep the main context for drafting. Give each agent a specific focus and ask for file:line citations.
- **Fresh-context review** (review-milestone): implementation and review should not share a context. Launch a `general-purpose` subagent that has NOT seen the implementation session to review the diff against the acceptance criteria and DESIGN.md conventions.
- **Model choice:** assume the main session usually runs on Opus. Delegate tasks that don't need Opus-level reasoning (fan-out searches, mechanical porting, formatting, boilerplate) to `model: "sonnet"` subagents to save cost. Never pass `model: "haiku"` for scoring logic or psychometric correctness — accuracy-critical work uses the default (inherited) model or stronger. If a task would genuinely benefit from a clean Fable-class session (e.g., independent review of accuracy-critical work), ask Jeff to launch one rather than attempting it yourself.

## Primary sources

When a primary source (paper, chapter, book, manual, or official scoring key) would help — verifying keying, cutoffs, scoring algorithms, or resolving an open question like OQ-1 — actively look for it (web search, DOI, publisher, OSF). If you cannot find or access it, ask Jeff whether he can upload it. Never substitute secondary descriptions or model memory for a primary source on keying or scoring content.

## Oracle strategy (tests must verify against ground truth, not against the code)

A test that asserts whatever the code currently produces (pure snapshots, shape-only checks) will enshrine bugs. Every scoring/validity test suite must include, in priority order:

1. **Hand-computed fixtures** — tiny synthetic datasets (2–3 respondents) scored by hand from the *published* scoring instructions, with exact expected values asserted. Keep the hand-worked arithmetic in comments so a human can re-derive it.
2. **Published reference values** — worked examples from official scoring materials or papers, when available (cite the source in the test).
3. **Independent recomputation** — recompute at least one scale per function inside the test with deliberately dumb, explicit code (hardcoded item numbers copied from the official key) and compare to the package's keying-table-driven result. This is the only check that catches transcription errors in `pid_items` itself.
4. **Invariant tests** — properties that must hold for any input: reverse-keyed items actually reversed, domain = mean of its 3 primary facets, a validity column present iff its scale was requested, output row count = input row count.

Snapshot tests are allowed only *on top of* the above (e.g., for cli message wording), never as the sole oracle for a numeric result.

## R package guardrails (apply during any implementation)

- After roxygen changes: `Rscript -e 'devtools::document()'`. After code changes: `Rscript -e 'devtools::test()'`.
- Never hand-edit `NAMESPACE`, `man/`, or `data/*.rda`; data regenerates via `data-raw/` scripts.
- README.md is knitted from README.Rmd (`devtools::build_readme()`).
- New tracked-file additions at repo top level need `.Rbuildignore` entries.
