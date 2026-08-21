# M44: A dedicated article for building and scoring HiTOP-SR modules

**Status:** done (2026-08-21, PR #49 https://github.com/jmgirard/hitop/pull/49)

**Goal:** Give the module workflow its own site article covering selection, generation,
fielding, and scoring end to end, and reduce the scoring tutorial's copy of it to a link.

**Outcome:** `vignettes/articles/modules-hitopsr.Rmd` ships as the Tutorials entry "Building
HiTOP-SR Modules", walking `available_scales()` → `hitop_module()` → the three generators →
name-based column selection against `ku_hitopsr` → `score_hitopsr()`/`reliability_hitopsr()`
with `module =`; every chunk evaluated. The 61-line copy in `vignettes/hitopsr_scoring.Rmd`
became a cross-reference under "Scoring Only Some Scales", a heading avoiding AC4's grep
terms. `hitop_module()`'s `@examples` gained the by-name idiom, closing the M37 candidate row
on it. Plus a `_pkgdown.yml` navbar row and a NEWS entry; no scoring behavior changed.

**Decisions:** none.

**Review:** three-lens fan-out; blame-history and prior-review clean (deleted content fully
reproduced, M43's positional-indexing lesson carried forward, empty GitHub comment probe).
The [O] diff-bug lens re-executed every factual claim, confirmed all, and reported ten
findings: six fixed on the branch (a `tempdir()` path leaked into the published page; the
negative α blamed on scale shortness; the unstated reason `module =` is required; a no-op
`#| warning: false`; no subscale caveat; one phrasing), three rejected as convention or
plan-owned text, one — three item-column naming patterns across the two datasets and
`rename_hitopsr_items()` — rejected as pre-existing and captured as a candidate row.
