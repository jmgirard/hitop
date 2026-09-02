# M084: The `label_*()` family's unpadded-item report

**Status:** done (2026-09-02, PR #91 https://github.com/jmgirard/hitop/pull/91)

**Goal:** The three `label_*()` helpers report a prefixed item column they could
not label whether or not any other column matched, and describe what they found.

**Outcome:** `warn_unpadded_items()` now runs ahead of the no-match early return
in all three `R/label_*.R` helpers, so a frame whose item columns are all
mis-padded raises the no-match warning and `hitop_unpadded_items` both.
`unpadded_item_cols()` (`R/util.R`) splits unmatched columns into `mispadded`
and `out_of_range`; `warn_unpadded_items(cols, prefix, expected, max_n,
instrument)` gives each its own sentence — the out-of-range one stating the
form's 1..max range — with `cli::qty()` before every plural marker, so each
pluralizes on its own count and the hint renders last. Roxygen, three `.Rd`
files, a `NEWS.md` bullet, +143 test assertions.

**Decisions:** D-058, annotating D-052's prefix-stripping consequence. Local: a
column both mis-padded and out of range is reported as out of range alone; the
no-match warning precedes the padding report.

**Review:** Three-lens fan-out (user-facing tier); no correctness bug. Six of ten
findings fixed at the gate (NEWS bullet, hint order, D-058, a numbering comment,
`at()` → `sentence_pos()`, a comment block), one to a candidate row, two
rejected, one stale; the "four remainders" row was dispositioned, not extended.
