# M058: The HiTOP-SR's NSSI scale is named in full

**Status:** done (2026-08-27, PR #64 https://github.com/jmgirard/hitop/pull/64)

**Goal:** The HiTOP-SR scale the package abbreviates `NSSI` carries its full name wherever the package prints or returns it, so the picker, the forms and the scored output agree.

**Outcome:** `Non-suicidal Self-injury` replaces `NSSI` in `data-raw/hitopsr_items.csv` and the four regenerated keying tables. The derived stem renames the scored columns to `hsr_nonSuicidalSelfInjury`/`_se`, which also move (448→451, 524→527) since the tables sort by name; no score changed. `hitop_module()` and `read_module()` no longer accept the abbreviation. Exactly two artifacts were rebuilt — `hitopsr_{US,A4}.docx` and their pkgdown copies — the Qualtrics and REDCap exports printing no scale name; `hitop_artifacts` gained two rows. Two checks ship: `data-raw/verify_hitopsr_scale_name.R` (committed string vs the source PDF's table cell, read at run time) and `data-raw/verify_hitopsr_rename.R` (five-step sweep and merge-base invariance). `definition_scale_labels` was deleted from `data-raw/hitopsr_info.R` as inert. `cairn/SOURCES.md` gained a scale-names provenance section and OQ-3; RB04/RR04 archived.

**Decisions:** D-041 — the source, the adopted string, the reconciliation commitment against the accepted paper, and the ruling that `hitopsr_definitions` is not evidence either way, its text being the maintainer's own.

**Review:** Pass 1 met all seven criteria but failed the gate on weight caps and a non-ISO date (defect return 1). Pass 2: criteria met on fresh evidence, gate clean, three-lens fan-out — both Sonnet lenses none, the Opus diff lens 14. Six fixed at the gate (two stale comments claiming the scale and definitions tables disagree; a build-script comment overclaiming its stem check; a source-file preamble calling a resolved question unresolved; NEWS's missing module break and two wording defects). One reverted — an also-known-as note for `?score_hitopsr`, since widening AC2's enumerated allow-list is a criterion amendment. Eight filed onto the `data-raw/` maintainer-tooling candidate row, clustered with M035's. None rejected.
