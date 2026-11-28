# M024: HiTOP-SR scale-subset generation (subset descriptor + docx/Qualtrics/REDCap)

**Status:** done (2026-07-30, PR #27 https://github.com/jmgirard/hitop/pull/27)

**Goal:** Let researchers select a subset of HiTOP-SR scales and generate DOCX/Qualtrics/REDCap instruments containing only those items, preserving original HSR item numbering.

**Outcome:** New `R/subset.R` exports `hitop_subset(instrument, scales)` — a validated S3 descriptor resolving scale names to the sorted union of their `hitopsr_scales$itemNumbers`, with parallel reverse flags and a `print()` method — plus the internal instrument-general reducer `apply_subset()`. `generate_docx_hitopsr()`, `generate_qualtrics_hitopsr()`, and `generate_redcap_hitopsr()` each gained `subset = NULL`. Item numbers are never rewritten, so subset-collected data still maps onto the full key. `include_subscales = TRUE` with `subset` errors. Incidental fix: `build_qualtrics_txt()`'s zero-pad width now derives from the largest item number rather than the row count (output-identical for all five Qualtrics generators, asserted). 82 new assertions; suite 9694 → 9779.

**Decisions:** Milestone-local (implement gate): dual scale-name vocabulary (display or camelCase, case-insensitive, verified collision-free); the padding fix in the shared helper rather than SR-only; `include_subscales` + `subset` rejected rather than silently narrowed. No cross-cutting D-entry.

**Review:** Three fresh lenses + scorer: 19 findings, 1 scored ≥ 80 and was actioned — a bypassable `isTRUE()` guard at `R/generate_docx.R:129` that let `include_subscales = 1` emit subscale rows listing items not on the form (score 85, fixed with a regression test). 18 logged below threshold. Blame and prior-review lenses: no conflicts. CI green on 7 jobs. Nothing retired.
