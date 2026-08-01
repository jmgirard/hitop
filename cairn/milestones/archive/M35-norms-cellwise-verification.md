# M35: Cell-by-cell verification of the shipped norms against the book

**Status:** done (2026-07-31, PR #38 https://github.com/jmgirard/hitop/pull/38)

**Goal:** Extend `data-raw/verify_norms_against_book.R` to diff every cell of the shipped
`pid_norms` against its independent extraction of the book's markup, closing the assembly stage that only scattered anchors observed.

**Outcome:** A third comparison in `verify_norms_against_book.R` reads the book's nine Appendix tables into `pid_norms`'s long format and diffs the shipped dataset — 4,606 rows over all 70 `(version, scale)` columns, keyed by `(version, scale)` plus T score, or raw score on the four validity scales the book prints without one.
Three separate NA-aware categories (rows in `pid_norms` only, rows in the book only, differing values), per-column counts, `stop()` on any.
Column identity comes from each table's banner row, and each spec entry's table index is checked against that table's `<caption>`.
The book-wording crosswalk is authored independently of `data-raw/norms_pid5.R` — facets by a case/`&` normalizing rule against `pid_scales$Facet`, domains by a five-entry banner map against `pid_domains` — so the two maps must agree rather than sharing one.
The 13 seeded corruptions moved to `data-raw/norms_mutations.R` with stable ids and a shared save/restore-by-hash wrapper; the new `data-raw/mutate_norms_book_check.R` runs them against the book comparison and all 13 are caught.
The layer map — exhaustive but markup-based and maintainer-run, versus the anchors as the only rendered-page layer, with CI seeing the anchors and structural invariants only — is stated in the script header and in `test-norms.R`'s block comment.

**Decisions:** none promoted. The plan gate's fixture-vs-script choice is a ROADMAP candidate row, not a D-entry.

**Review:** Three fresh-context lenses plus a scorer; blame-history and prior-review found nothing blocking.
Nothing reached the 80 actioning threshold, so all 15 findings are logged with scores rather than actioned.
The 70/68/65 cluster — how the script reports a failure it does detect — graduated to a candidate row; the 72 finding corrected the M34 in-place-swap lesson to name both harnesses.
