# M036: Two-line response-option legend on the PID paper forms

**Status:** done (2026-07-31, PR #39 https://github.com/jmgirard/hitop/pull/39)

**Goal:** The PID paper forms print their response-option legend on two lines of two options each, so no option phrase is broken mid-phrase by column wrapping, while the HiTOP-SR/BR forms keep their single-line legend and no legend wording changes.

**Outcome:** `make_items_table()` gained `opts_per_line`, defaulting to `nrow(opts)` so every existing caller is unchanged; the legend is built by striding `pairs` and emitting one `add_header_lines()` value per group. `generate_docx_pid5()`, `generate_docx_pid5sf()` and `generate_docx_pid5bf()` pass 2; SR/BR pass nothing. The same helper's even-row shading index moved from `seq(2, n, by = 2)` to `which(seq_len(n) %% 2 == 0)`, fixing an abort at n = 1 ("wrong sign in 'by'") and agreeing with the old index at every n >= 2. Two `word/document.xml` parsers (`docx_legend_lines`, `docx_legend_pairs`) and seven tests in `test-generate_docx.R`, all verified red by mutation. The six committed PID DOCX regenerated, `hitop_artifacts` 26 -> 32 rows, NEWS entry.

**Decisions:** D-028 (the split is layout, not IP1 content). AC1's evidence split into a one-time merge-base comparison plus a lasting test against the committed SR/BR forms, because `R CMD check` builds from a tarball with no `.git`. AC9 added mid-implementation at the maintainer's direction.

**Review:** Blame-history and prior-review lenses zero findings; diff-bug lens 14, one at 80+. F4 (82) fixed: the "not a hardcoded four" test used a three-option table, where `seq(1, 3, by = 4)` also yields one line, so it passed against the mutation it named — moved to five options and re-verified red. AC7's visual proof was a QuickLook render, not Word (Word could not be driven from the session); the maintainer accepted it at the merge gate with the renderer named. F1 (32, fractional `opts_per_line` drops an option) and F12 (48, AC9's reachability overclaimed) logged unactioned.
