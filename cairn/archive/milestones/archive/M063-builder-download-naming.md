# M063: Every file the builder writes says which form it belongs to

**Status:** done (2026-08-29, PR #70 https://github.com/jmgirard/hitop/pull/70)

**Goal:** Name every download for the format that built it, whether it covers the whole
questionnaire, and whether a Word form was shuffled, so two builds in one session cannot
be confused or shadow each other's descriptor.

**Outcome:** `downloadStem(format, whole, shuffle)` in the builder's `index.html` composes one
stem from `FORMATS[].name` (`word`/`qualtrics`/`redcap`), `-module` on a scale selection and
`-shuffled` on a shuffled Word form, and `download()` gives the questionnaire and its descriptor
that same stem: the eight format x scope x shuffle builds produce sixteen distinct names, each
pair differing only in extension. The README gained a *What the downloads are named* section
tabulating all eight pairs, and the page notice says what a name carries and what it does not.
Shipped in jmgirard/hitop-builder#9 (`afb1535`), deployed byte-identical; the `hitop` package is
untouched. Scale selection, paper size, item numbering and the Qualtrics/REDCap naming values
stay readable only in the descriptor, so two selections in one format still share a name.

**Decisions:** M063-D1 — the stem is the instrument, the format's word, `-module`, `-shuffled`; milestone-local.

**Review:** three-lens fan-out; prior-review and blame-history found no regression, the diff-bug
lens found `downloadStem` correct over every reachable combination and eight prose findings. Four
fixed before the merge (two false "two builds never share a name" claims, the self-contradicting
`downloadStem` comment, one over-long line), three deferred to the builder-page candidate row at
Jeff's disposition, none rejected. No criterion failed; the return floor did not fire.
