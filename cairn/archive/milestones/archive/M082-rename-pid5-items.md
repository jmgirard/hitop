# M082: `rename_pid5_items()`

**Status:** done (2026-09-02, PR #89 https://github.com/jmgirard/hitop/pull/89)

**Goal:** Ship `rename_pid5_items()`, so PID-5 data named elsewhere — or named by this package
before D-055 — can be renamed into the pattern D-055 fixed.

**Outcome:** `R/rename_pid5_items.R` exports `rename_pid5_items(data, version, method, item_cols, item_text,
from_prefix, prefix)`. `method = "number"` (the default) renames columns spelled
`<from_prefix><digits>` (`from_prefix` defaults to `"pid_"`, the package's pre-D-055 spelling) to
the canonical padded name of the form named by `version`, the three forms numbering independently;
`method = "text"` matches `item_text` against that form's `pid_items$Text` rows under `trimws()`, as
the HiTOP sibling does. `prefix = NULL` resolves to `pid5_`, `pid5sf_` or `pid5bf_`, padded via
`item_names()` to the form's largest. Both methods report skipped inputs through
`warn_unmatched_items()` in `R/util.R`, raising the public class `hitop_unmatched_items` and
doubling braces in caller-supplied text so cli reads none as an expression. `test-
rename_pid5_items.R` covers all three forms; NEWS, `_pkgdown.yml` and the vignettes carry it.

**Decisions:** D-057 records the shared condition class, the sibling's warning left uneven.

**Review:** three-lens fan-out; prior-review and blame-history returned nothing, the diff-bug lens
16 ranked findings. All six criteria passed on fresh evidence. Seven fixed on the branch — chiefly
that nothing said the digits are read as the *named form's* item number, an overstated NEWS claim,
the class absent from `@return`, hardcoded 220/100/25 counts; one rejected (the mutated membership
test is extensionally equivalent on contiguous 1..N numbering); eight filed to the candidate row.
