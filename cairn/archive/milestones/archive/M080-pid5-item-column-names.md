# M080: The shipped PID-5 datasets name item columns as the online exports do

**Status:** done (2026-09-02, PR #86 https://github.com/jmgirard/hitop/pull/86)

**Goal:** The four shipped PID-5 datasets name their item columns the way this
package's own REDCap export writes them, so data fielded through it scores
without renaming and every PID-5 example uses one selection idiom.

**Outcome:** `sim_pid5`, `sim_pid5sf`, `sim_pid5bf` and `ku_pid5sf` carry
`pid5_001`..`pid5_220`, `pid5sf_001`..`pid5sf_100` and `pid5bf_01`..`pid5bf_25`,
values and positions unchanged against the objects pinned at `d3ac6695`.
`data-raw/rename_pid_item_columns.R` renames, and also rewrites `ku_pid5sf`'s
readr `spec`, which recorded the 100 old names one attribute below `names()`;
`sim_pid.R` and the `ku_pid5sf.csv` header regenerate the same names; new
`check_pid_item_names.R` sweeps `data/*.rda`, `R/sysdata.rda` and the members
and bodies of every `inst/extdata/` archive. Every call site moved, and NEWS
gives all three forms' old and new spelling with the migration idiom.

**Decisions:** none cross-cutting; per-form stems and the waived deprecation
cycle were settled at the plan gate under D-052's precedent.

**Review:** two rounds; the first failed AC4 (a comment spelling an old name)
and AC5 (a missing old spelling in NEWS), the second passed all six criteria on
fresh evidence with the gate green. Of thirteen findings across the two rounds
none was an AC failure: six were fixed on the branch, five filed, two rejected.
