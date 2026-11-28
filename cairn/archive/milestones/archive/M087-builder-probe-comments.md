# M087: The builder's start-up probe comments state what the probe establishes

**Status:** done (2026-09-03, PR #95 https://github.com/jmgirard/hitop/pull/95)

**Goal:** The comments on the builder page's start-up `tilesExactly` probe say what it establishes, not an overlap check `hitop_module()` makes impossible.

**Outcome:** The three comment blocks in `jmgirard/hitop-builder`'s `index.html` — above `tilesExactly`,
above `wholeInstrument()`, above the R call — rewritten in that repo's PR #13, merged at `bfc5feb`;
comment lines only, 43 inserted and 13 deleted, no page behavior, README or test touched. Each states
the promise as a gap-free run of item numbers from 1 whose own length is the only item count it reads,
and names both things it does not establish: overlap, because
`hitop_module()` sorts and de-duplicates at `R/module.R:102`, so scales sharing items yield the same
union; and a tail of higher-numbered items no scale claims, because `seq_along()` measures the union
against itself. The `as.integer()` stays, its reason in the block.

**Decisions:** M087-D1 (the probe establishes the union is 1..N, not the absence of overlap), superseded
by M087-D2 (a gap-free run from 1, not coverage — nothing reads the instrument's own item count). No
lesson captured: the transferable point, deriving a claim from the expression rather than the
identifier, is the rulebook's derived-claims rule.

**Review:** Two passes, three lenses each; blame-history and prior-review returned none in both. Pass 1
returned a defect on AC1 — the comments still claimed the probe catches uncovered items — repaired as T5
with AC1 amended. Pass 2's [O] lens returned six: two fixed on the branch (a closing clause asserting
the coverage its own block disclaims, a FALSE-branch claim holding only where an instrument's numbering
is itself 1..N), two rejected, two to the candidate row replacing this milestone's own. Returns: 1.
