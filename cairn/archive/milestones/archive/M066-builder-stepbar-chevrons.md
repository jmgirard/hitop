# M066: The step bar shows its three buttons run in order

**Status:** done (2026-08-29, builder PR #8 https://github.com/jmgirard/hitop-builder/pull/8, squash-merged `efea175`, Pages deploy green on that SHA)

**Goal:** Make the builder's three step buttons read as a sequence rather than three separate controls.

**Outcome:** In `jmgirard/hitop-builder`'s `index.html`, `#stepbar li` becomes a flex row and `#stepbar li + li::before` draws a `clip-path` triangle before the second and third steps. The pseudo-element's `content` is empty, so it adds no node to the accessibility tree and no character for a screen reader to announce. Under 30rem, where the steps stack, the list item stacks too: the chevron rotates 90 degrees and sits above the button it points at, leaving all three buttons on one left edge. CSS only; no script, no markup, no R package change.

**Tier:** none. This ran as ad-hoc conversational work, not through any work tier — no milestone file, no plan gate, no branch record, no review fan-out. It changes what a visitor sees, so the trivial tier did not cover it, and it fixed no bug, so `/hotfix` did not either. Under the tiers as written it was a (small) milestone and should have been planned as one. This entry is the catch-up record; M066 was opened at the 2026-08-29 health audit, after the merge, at Jeff's disposition.

**Work log (catch-up, reconstructed from git and this session):** branch `stepbar-chevrons`, two commits — `6dc8b8a` added the chevron; `2e91818` fixed the stacked layout, where the list item had stayed a flex row so the rotated chevron sat beside its button and pushed steps 2 and 3 in by 14px against step 1, giving a ragged left edge. The defect was found by driving the merged-with-main page in a browser at 375px (buttons at x=16, 30, 30) and confirmed fixed there (all three at x=16, width 343); the wide layout was re-checked unchanged at 1000px (one row, buttons at x=92, 381, 655). The PR body had claimed the stacked buttons stayed aligned; it was corrected before the merge.

**Decisions:** none, milestone-local or cross-cutting.

**Review:** none ran. The merge was blocked in this session by the cairn merge guard, which keys on the session working directory and so checked `hitop`'s marker rather than the builder repo's; Jeff merged from his own terminal after another session resolved a conflict with main. The guard's cross-repo blind spot is the reason this work reached a distribution channel with no approval marker and no review evidence — recorded here, not yet dispositioned.
