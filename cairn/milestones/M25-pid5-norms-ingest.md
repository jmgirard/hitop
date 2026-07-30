# M25: PID-5 normative tables — verification and ingest

- **Status:** blocked
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3
- **Branch/PR:** `m25-pid5-norms-ingest`

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
      reconciled against the book: an independent transcription is produced from the
      source by a route that did not produce the CSVs — a committed, re-runnable
      extractor over the book's table markup, or a fresh reader where no markup exists —
      and the cell-by-cell diff of that transcription against the CSVs is recorded in
      this file, with every discrepancy either corrected in the CSV with maintainer
      sign-off (IP1) or recorded as printed-in-source.
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

- [x] **T1.** Maintainer uploads the book's norm-table pages to `cairn/references/sources/`
      (gitignored shelf); confirm the pages cover all seven tables. Blocks T2.
- [x] **T2.** Independent re-transcription of those pages by a fresh reader that has not
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
- 2026-07-30: implementation started on `m25-pid5-norms-ingest`.
- 2026-07-30: T1 done — the maintainer uploaded the whole book as `markon2024.epub` (Markon, Fossati, Somma & Krueger, 2024, APA Publishing, ISBN 9781615375127) rather than page scans. Its Appendix carries 12 tables, of which the seven M25 ships are A-1 (SRF VRIN), A-2 (100-item VRIN), A-3 (ORS), A-4 (PIM-RD), A-5 (SRF domain), A-7 (SF domain), A-9 (BF total+domain); the epub is paginated (196 page anchors), so per-table page anchors are available for AC2.
- 2026-07-30: the five remaining appendix tables are out of M25 scope as planned and were captured as ROADMAP candidates on main before branching — A-6/A-8 (facet norms, extending the existing facet candidate) and A-10/A-11/A-12 (Informant Form, a new candidate; the package has no IRF surface).
- 2026-07-30: gate chose the committed-extractor route for AC1 over a fresh reader retyping, because the book arrived as structured markup rather than page scans; AC1's method wording is amended accordingly (see Decisions). Falsified by table markup that turns out not to be losslessly extractable — e.g. values carried in images or in flowed text rather than cells.
- 2026-07-30: T4's mapping is evidenced from the book's own attributions — A-1 to Keeley et al. (2016) = `INC`, A-2 to Lowmaster et al. (2020/2021) = `INCS` (the book's caption says VRIN but its text names PID-5-INC-S), A-3 to Sellbom et al. (2018) = `ORS`, A-4 to Williams et al. (2019) = `PRD`. Each matches the source that package column already cites; maintainer sign-off still pending behind RB01.
- 2026-07-30: blocked on RB01 — the maintainer asked whether the package should harmonize validity-scale naming to the book (`INC` to `VRIN`); that is an irreversible exported-API decision (RB tripwire `irreversible-api`), so it goes to a Fable review rather than being settled in-session. RB01 is committed on this branch rather than the default branch, because the milestone file it blocks is ahead of main here.
- 2026-07-30: T2 done — `data-raw/verify_norms_against_book.R` extracts all seven tables from the book's own table markup and diffs them cell by cell against the CSVs. Row identity is established by the printed T/score column rather than by position, so the extractor cannot silently misalign; all seven tables match the CSVs on dimensions and on every T value. Page anchors read off the epub's pagebreak ids: A-1 p. 116, A-2 p. 117, A-3 p. 117, A-4 p. 118, A-5 p. 120, A-7 p. 147, A-9 p. 174.
- 2026-07-30: [O] criteria audit ran twice (pre- and post-gate). Pass 1: findings on all 11 drafted criteria. Pass 2 on the revised 12: 8 OK, 4 findings — undefined `check()` NOTE baseline (M25 AC5, M26 AC7), validity scales carrying no T (M26 AC2, AC4), non-injective raw→T (M26 AC4), tripwire branch leaving M26 AC5 unsatisfiable. All four fixed before writing; none escalated to a second gate round.

## Decisions

- 2026-07-30 (M25-D1): AC1's transcription route amended from "a reader that did not produce the CSVs transcribes the uploaded source pages" to "a committed, re-runnable extractor over the book's table markup, or a fresh reader where no markup exists". The plan assumed page scans; the book arrived as an EPUB whose appendix tables are well-formed markup carrying one cell per value. A deterministic extractor removes the transcriber-error class the plan's own falsification note named, and satisfies the reproducibility hard stop by regenerating the transcription from scratch rather than pinning a typed copy. Independence is preserved in the sense AC1 cares about: the extractor reads the book and never the CSVs. Maintainer approved at the implementation question gate; falsified by markup that proves not losslessly extractable (values in images, or flowed text rather than cells).

## Review
