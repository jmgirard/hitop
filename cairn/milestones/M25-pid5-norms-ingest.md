# M25: PID-5 normative tables — verification and ingest

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3
- **Branch/PR:** —

## Goal

Ship the maintainer-transcribed PID-5 / PID-5-SF / PID-5-BF normative tables as one
verified, source-cited, documented dataset with oracle tests.

## Scope

**In:** the seven `data-raw/norms_*.csv` tables; independent re-transcription of the
book's norm-table pages and cell-by-cell reconciliation; maintainer confirmation of
the validity-scale name mapping; a `data-raw/norms_pid5.R` script building an exported
long-form `pid_norms`; `cairn/SOURCES.md` provenance plus a `cairn/references/` source
note and INDEX line; `tests/testthat/test-norms.R`; `R/data.R` docs, `_pkgdown.yml`
entry, NEWS.

**Out:** `norm_pid5()` and any raw/T/percentile conversion → M26. The PID-5-BF total
scale in `score_pid5()` (D-017) → M26. Response-range rescaling at lookup → M26
(absorbs the legacy-M16 candidate). Facet-level, sex/age-stratified, and HiTOP-SR/BR
norms → ROADMAP candidate rows. Profile plots and rendered reports → the existing
"Clinical reporting & release" candidate.

## Acceptance criteria

- [ ] **AC1.** Every numeric cell of the seven committed `data-raw/norms_*.csv` tables is
      reconciled against the book: a reader that did not produce the CSVs transcribes
      the uploaded source pages independently, and the cell-by-cell diff of that
      transcription against the CSVs is recorded in this file, with every discrepancy
      either corrected in the CSV with maintainer sign-off (IP1) or recorded as
      printed-in-source.
- [ ] **AC2.** `cairn/SOURCES.md` carries a PID-5 norms section giving the book's full
      citation, a page/table anchor for each of the seven tables, the normative sample
      as the book describes it, the maintainer-confirmed mapping from each book scale
      name (VRIN, ORS, PIM-RD) to the package column it norms — or, where the
      maintainer's check finds no correspondence, a record that the table is not
      shipped and why — and a per-table verification status; and a citekey-named
      source note under `cairn/references/` exists carrying a Provenance block and its
      `INDEX.md` line.
- [ ] **AC3.** `pid_norms` is a single exported tibble in long form — one row per
      (version, scale, T) for the three domain tables and per (version, scale, score)
      for each validity table AC2 records as shipped — and `data-raw/norms_pid5.R`
      regenerates `data/pid_norms.rda` from the CSVs such that re-running it leaves
      `data/` byte-unchanged.
- [ ] **AC4.** `tests/testthat/test-norms.R` fails when any one of these mutations is
      applied to `pid_norms`, each mutation run with its failure recorded as review
      evidence: (a) any domain `raw` value changed by ≥ 0.02, breaking the per-scale
      invariant `raw == max(0, a + b*T)` to a tolerance of 0.006, with `a`/`b` fit on
      the rows above that scale's zero floor; (b) any `percentile` changed so it falls
      below its predecessor within a scale; (c) any `percentile` set outside [0, 1];
      (d) any one of the ≥ 15 values hardcoded in the test from named book pages.
- [ ] **AC5.** `Rscript -e 'devtools::document()'` leaves the working tree clean and
      `Rscript -e 'devtools::check()'` reports 0 errors and 0 warnings, with every NOTE
      justified in this file's Review section (`cairn/PROFILE.md` consistency-gate), and
      with `pid_norms` documented in `R/data.R` and listed under "Instrument Data" in
      `_pkgdown.yml`.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T4, T5
- AC3 → T6
- AC4 → T7
- AC5 → T8

## Tasks

- [ ] **T1.** Maintainer uploads the book's norm-table pages to `cairn/references/sources/`
      (gitignored shelf); confirm the pages cover all seven tables. Blocks T2.
- [ ] **T2.** Independent re-transcription of those pages by a fresh reader that has not
      seen the CSVs; emit a machine-diffable transcription.
- [ ] **T3.** Diff the transcription against the seven CSVs cell by cell; reconcile each
      discrepancy (correct with sign-off, or record as printed-in-source) and log the
      ledger here.
- [ ] **T4.** Maintainer confirms the book-scale → package-column mapping for
      `norms_pid5_vrin`, `norms_pid5sf_vrin`, `norms_pid5_ors`, `norms_pid5_pimrd`
      against the book's own scale descriptions; any table with no confirmed
      correspondence is held out of `pid_norms`.
- [ ] **T5.** Write the `cairn/SOURCES.md` PID-5 norms section, the citekey source note
      from `templates/source-note.md`, and its `cairn/references/INDEX.md` line.
- [ ] **T6.** Write `data-raw/norms_pid5.R` building long-form `pid_norms` from the CSVs;
      regenerate `data/`. (M18 lesson: `readr`/`usethis` must be installed locally.)
- [ ] **T7.** Write `tests/testthat/test-norms.R` — the three structural invariants plus
      ≥ 15 page-cited spot values — and run each mutation to confirm it reddens.
- [ ] **T8.** Document `pid_norms` in `R/data.R`, add the `_pkgdown.yml` entry and a NEWS
      line; run `document()` / `test()` / `check()`.

## Work log

- 2026-07-30: created by /milestone-plan.
- 2026-07-30: plan-time structural screen (raw linear in T; percentile monotone in [0,1]) flagged 3 cells; maintainer checked the book and corrected all 3 in `data-raw/` before planning closed.
- 2026-07-30: plan gate chose independent re-transcription of the book pages over a maintainer second pass or a ~20-cell spot check, because a monotone-preserving percentile typo survives the structural screen; falsified by a re-transcription diff whose discrepancies are all traced to the transcriber rather than the CSVs.
- 2026-07-30: plan chose one long-form `pid_norms` tibble over seven wide per-table datasets, because facet-level and stratified norms are expected later and a long table absorbs them without new exports; falsified by a lookup path that needs per-table column layout for acceptable performance or ergonomics.
- 2026-07-30: the three corrections landed directly on main (c995586) rather than a branch — no runtime surface, nothing reads `data-raw/norms_*.csv` yet; the git-model hook flagged the non-`cairn/` path and the trivial-tier call is recorded here.
- 2026-07-30: [O] criteria audit ran twice (pre- and post-gate). Pass 1: findings on all 11 drafted criteria. Pass 2 on the revised 12: 8 OK, 4 findings — undefined `check()` NOTE baseline (M25 AC5, M26 AC7), validity scales carrying no T (M26 AC2, AC4), non-injective raw→T (M26 AC4), tripwire branch leaving M26 AC5 unsatisfiable. All four fixed before writing; none escalated to a second gate round.

## Decisions

## Review
