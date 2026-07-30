# M25: PID-5 normative tables — verification and ingest

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR01
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
      name (VRIN, ORS, PID-5-PRD) to the package column it norms — or, where the
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

- [ ] **AC6 (BC1).** For each validity table that ships in `pid_norms`, its `scale`
      value is exactly the package column stem — `"INC"` (book Table A-1), `"INCS"`
      (A-2), `"ORS"` (A-3), `"PRD"` (A-4) — and no `scale` value anywhere in
      `pid_norms` is `"VRIN"`, `"VRINS"`, `"INC-S"`, or `"PIM-RD"`.
- [ ] **AC7 (BC2).** M25 introduces no rename of existing validity-scale names on any
      exported surface: `validity_pid5()` output column names and the
      `pid_items`/`data-raw/pid_items.csv` validity columns (`INC`, `INCS`,
      `ORS`, `ORSS`, `PRD`, `PRDS`, `SDTD`, `SDTDS`) are unchanged from their
      pre-M25 state.
- [ ] **AC8 (BC3).** The `cairn/SOURCES.md` PID-5 norms section required by AC2 records,
      with table/page anchors: (a) the A-2 caption name "Variable Response
      Inconsistency (VRIN)" *and* the Chapter 4 name "PID-5-INC-S" as an internal
      inconsistency of the book, with the mapping to `INCS` citing the chapter text
      and Lowmaster et al. (2020); and (b) the book's abbreviation PID-5-PRD (not
      "PIM-RD") mapped to package `PRD`.
- [ ] **AC9 (BC4).** The `pid_norms` roxygen block in `R/data.R` carries a one-sentence
      note that the `INC`/`INCS` scales are also called the Variable Response
      Inconsistency (VRIN) scale by Markon et al. (2024), so that a reader of the book
      can find the columns.

### Deviations from RR01

| RR01 text | Departure | Why |
|---|---|---|
| BC1 "the `scale` values for the four validity tables are exactly …" | AC6 makes it conditional: "For each validity table that ships in `pid_norms`, its `scale` value is exactly …" | BC1 as written asserts all four validity tables ship, foreclosing the hold-out branch AC2, AC3 and T4 deliberately preserve; if T4 had found no correspondence for a table, satisfying AC2 would have violated BC1. |
| BC1 "…, or any other book-caption spelling." | Clause dropped; AC6 keeps the four named literals. | Unenumerable, so no verifier could ever confirm it. |
| BC3 "justified from the chapter text and Lowmaster et al. (2020/2021)" | AC8 reads "citing the chapter text and Lowmaster et al. (2020)". | "Justified" sets no observable failure threshold, so it is restated as a presence test; and the package's own citation in `R/validity_pid5.R` is Lowmaster 2020, leaving the 2020/2021 pairing unresolved. |
| RR01 §7 "no AC wording change needed" | AC2's parenthetical amended from "(VRIN, ORS, PIM-RD)" to "(VRIN, ORS, PID-5-PRD)". | RR01 itself establishes the book never writes "PIM-RD"; leaving AC2 as it stood would make AC2 and BC3 jointly unsatisfiable. |
| — (no BC) | AC9 added, binding RR01 Recommendation 3's `pid_norms` book-name note. | The recommendation is marked Apply but no BC or existing AC covered it, leaving the review's only user-facing deliverable with no verifier. |

All five departures come from the fresh-context [O] binding-criteria audit run before
ingestion (2026-07-30); BC2 is ingested verbatim as AC7.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T4, T5
- AC3 → T6
- AC4 → T7
- AC5 → T8
- AC6 → T6
- AC7 → T6, T8
- AC8 → T5
- AC9 → T8

## Tasks

- [x] **T1.** Maintainer uploads the book's norm-table pages to `cairn/references/sources/`
      (gitignored shelf); confirm the pages cover all seven tables. Blocks T2.
- [x] **T2.** Independent re-transcription of those pages by a fresh reader that has not
      seen the CSVs; emit a machine-diffable transcription.
- [x] **T3.** Diff the transcription against the seven CSVs cell by cell; reconcile each
      discrepancy (correct with sign-off, or record as printed-in-source) and log the
      ledger here.
- [x] **T4.** Maintainer confirms the book-scale → package-column mapping for
      `norms_pid5_vrin`, `norms_pid5sf_vrin`, `norms_pid5_ors`, `norms_pid5_pimrd`
      against the book's own scale descriptions; any table with no confirmed
      correspondence is held out of `pid_norms`.
- [x] **T5.** Write the `cairn/SOURCES.md` PID-5 norms section, the citekey source note
      from `templates/source-note.md`, and its `cairn/references/INDEX.md` line.
- [x] **T6.** Write `data-raw/norms_pid5.R` building long-form `pid_norms` from the CSVs;
      regenerate `data/`. (M18 lesson: `readr`/`usethis` must be installed locally.)
- [x] **T7.** Write `tests/testthat/test-norms.R` — the three structural invariants plus
      ≥ 15 page-cited spot values — and run each mutation to confirm it reddens.
- [x] **T8.** Document `pid_norms` in `R/data.R`, add the `_pkgdown.yml` entry and a NEWS
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
- 2026-07-30: T3 done — 18 cells corrected against the book with maintainer sign-off; the verifier now reports all seven tables matching cell for cell. Ledger in Decisions (M25-D2).
- 2026-07-30: RR01 returned and was ingested — keep `INC`, no rename; see Decisions (M25-D3, M25-D4) and D-018. Its binding criteria were audited first by a fresh-context [O] reader, which found BC1 foreclosed the table hold-out branch AC2/AC3/T4 preserve, BC3 collided with AC2's own "PIM-RD" wording, and RR01's only user-facing recommendation had no verifier; all five fixes applied as recorded deviations. T4 discharged by the maintainer's acceptance. RB01/RR01 archived; M25 back to in-progress.
- 2026-07-30: ingesting RR01 took M25 to 9 acceptance criteria, past the 7-criterion split tripwire (`cairn_validate` sizing advisory). Not split: AC7 is a restrictive no-op guard over files M25 never touches, AC8 records content AC2/T5 already produce, and AC9 is one sentence in the roxygen block T8 writes — the criteria count rose without the work volume rising, and all four map onto existing tasks.
- 2026-07-30: T5 done — `cairn/SOURCES.md` gains a PID-5 normative tables section (citation, per-table page anchors, the sample as the book states it, the book-label to package-column mapping, and per-table verification status), the source note `cairn/references/markon2024.md` is written from the template with a Provenance block, and its INDEX line is added. The book's own domain-norm inclusion criterion (VRIN < 17) matches the package's documented `INC` cut score of 17, which corroborates the A-1 mapping independently of the shared Keeley attribution.
- 2026-07-30: T6 done — `data-raw/norms_pid5.R` builds `pid_norms`, 1,056 rows over 5 columns (16 domain scale-by-version blocks and 4 validity scales); re-running the script leaves `data/pid_norms.rda` byte-identical (same md5) and `devtools::test()` is clean. Shape choices at the implementation gate are in Decisions (M25-D5).
- 2026-07-30: T7 done — `tests/testthat/test-norms.R` (155 expectations) carries the three structural invariants, a scale-coverage and validity-naming guard for AC6/AC7, and 33 spot values transcribed from the seven printed tables with page anchors. `data-raw/mutate_norms_check.R` makes AC4's mutations re-runnable: all six (two AC4(a), one each AC4(b) and AC4(c), two AC4(d)) redden the file and the dataset is restored byte-identical. The harness first left `data/pid_norms.rda` mutated because `on.exit()` at an Rscript's top level fires at the end of its own statement, not at script end; the restore now runs inside a function.
- 2026-07-30: T8 done — `pid_norms` roxygen block in `R/data.R` (format, the normative sample as the book states it, the AC9 VRIN note, `@source` with the table anchors), `_pkgdown.yml` "Instrument Data" row, NEWS bullet. `document()` leaves the tree clean, `check()` reports 0 errors / 0 warnings / 0 notes, `pkgdown::check_pkgdown()` finds no problems.
- 2026-07-30: [O] criteria audit ran twice (pre- and post-gate). Pass 1: findings on all 11 drafted criteria. Pass 2 on the revised 12: 8 OK, 4 findings — undefined `check()` NOTE baseline (M25 AC5, M26 AC7), validity scales carrying no T (M26 AC2, AC4), non-injective raw→T (M26 AC4), tripwire branch leaving M26 AC5 unsatisfiable. All four fixed before writing; none escalated to a second gate round.

## Decisions

- 2026-07-30 (M25-D1): AC1's transcription route amended from "a reader that did not produce the CSVs transcribes the uploaded source pages" to "a committed, re-runnable extractor over the book's table markup, or a fresh reader where no markup exists". The plan assumed page scans; the book arrived as an EPUB whose appendix tables are well-formed markup carrying one cell per value. A deterministic extractor removes the transcriber-error class the plan's own falsification note named, and satisfies the reproducibility hard stop by regenerating the transcription from scratch rather than pinning a typed copy. Independence is preserved in the sense AC1 cares about: the extractor reads the book and never the CSVs. Maintainer approved at the implementation question gate; falsified by markup that proves not losslessly extractable (values in images, or flowed text rather than cells).

- 2026-07-30 (M25-D2): AC1 reconciliation ledger and IP1 sign-off. The extraction found 18 disagreeing cells across the seven tables; all 18 are percentile values, and every one is monotone-preserving, so the plan-time structural screen could not have reached them — the plan gate's reason for requiring an independent re-transcription. Every discrepancy shares one signature: the CSV holds the book's value from one row below. In `norms_pid5sf_domains.csv` this displaces the whole `ANT_Ptl` column from T=48 down (the column matches the shifted reading on 53 of 60 rows against 44 for the true reading; only the 16 rows below expose it, because wherever two consecutive book values are equal a one-row shift is invisible). `ANT_Raw` matches the book on every row, so the slip is confined to one column rather than a misaligned row. Maintainer signed off on correcting all 18 to the book's printed values (2026-07-30), confirming separately that the book is correct for the two A-5 cells. Corrections applied, one field per line, nothing else in either file touched:
  `norms_pid5sf_domains.csv` `ANT_Ptl` (book A-7, p. 147), CSV→book: T=48 0.58→0.52, T=50 0.64→0.58, T=51 0.68→0.64, T=53 0.74→0.68, T=55 0.78→0.74, T=56 0.79→0.78, T=57 0.82→0.79, T=58 0.85→0.82, T=60 0.87→0.85, T=62 0.89→0.87, T=64 0.91→0.89, T=65 0.92→0.91, T=67 0.94→0.92, T=69 0.96→0.94, T=72 0.97→0.96, T=74 0.98→0.97.
  `norms_pid5_domains.csv` `DET_Ptl` (book A-5, p. 120), CSV→book: T=72 0.98→0.97, T=76 0.99→0.98.
  Re-running `data-raw/verify_norms_against_book.R` after the corrections reports every cell of all seven tables matching the book. No discrepancy was dispositioned as printed-in-source.

- 2026-07-30 (M25-D3): RR01 ingested. Its answer to the escalated question is that the package keeps `INC`/`INCS` and does not harmonize to the book's "VRIN", on three grounds: the book is internally inconsistent (both A-1 and A-2 captions say VRIN while Chapter 4 calls the 100-item scale PID-5-INC-S six times), the development papers both title the scale "Response Inconsistency Scale" and Lowmaster's own PID-5-INC-S treats `INC` as the parent stem, and no rename of the pair is consistent with the book, the papers, and internal lineage at once. The cross-cutting half is promoted to [D-018](../DECISIONS.md); the M25-local half is that `pid_norms$scale` carries package column stems (AC6), which M26's lookup then joins by string equality with no crosswalk. Maintainer accepted 2026-07-30; that acceptance also discharges T4's IP1 sign-off on the book-scale to package-column mapping, which the binding-criteria audit asked be stated rather than assumed.
- 2026-07-30 (M25-D4): RR01's Q7 correction stands against this milestone's own plan text — the book's abbreviation for the Williams scale is PID-5-PRD (15 occurrences in Chapter 4) and never "PIM-RD" (zero). AC2's parenthetical is amended accordingly and the CSV filename `norms_pid5_pimrd.csv` is deliberately left alone: renaming data-raw files is not in M25's scope, and `SOURCES.md` will carry the filename-to-book-label mapping, so the record explains the mismatch rather than the filename hiding it.

- 2026-07-30 (M25-D5): `pid_norms` column names and row layout. The exported tibble is `version` / `scale` / `tscore` / `raw` / `percentile` — 1,056 rows, one per (version, scale, T) for the three domain tables and one per (version, scale, raw score) for the four validity tables, whose rows carry `tscore = NA` because the book prints no T for them. AC3's "(version, scale, T)" and "(version, scale, score)" name the identifying quantities rather than column names; a validity raw score is a raw score, so it is carried by `raw` and no fifth identifying column ships. The T column is `tscore` and not `T` by the maintainer's choice at the implementation question gate, because `T` is base R's shorthand for `TRUE` and a data column of that name reads as a collision even where masking makes it work. No book-table anchor column ships either (same gate): the per-table page anchors live in `cairn/SOURCES.md`, which owns provenance. `scale` carries score-output column stems — the five domain stems read from `pid_domains$camelCase` rather than retyped, `"total"` for the BF whole-form score M26 adds under D-017, and the four `validity_pid5()` stems `INC` / `INCS` / `ORS` / `PRD` (M25-D3, D-018).

## Review
