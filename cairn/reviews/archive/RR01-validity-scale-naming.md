# RR01: Validity-scale naming — findings

- **Date:** 2026-07-30
- **In response to:** `cairn/reviews/RB01-validity-scale-naming.md`
- **Reviewer:** independent Fable review (fresh session, brief-directed reading only)

Materials read: `R/validity_pid5.R`; `R/data.R` (validity-column docs);
`data-raw/pid_items.csv` (header); `tests/testthat/test-validity_pid5.R`,
`test-keying.R`, `helper-fixtures.R`; `cairn/SOURCES.md`; `cairn/DESIGN.md`
IP1–IP4/GP1–GP4; `cairn/milestones/M25-pid5-norms-ingest.md` (Goal/Scope);
`NEWS.md` (0.2.0); `DESCRIPTION` (Imports); and the book itself
(`cairn/references/sources/markon2024.epub` — Appendix table captions and
Chapter 4 term usage, extracted directly from the XHTML).

## Answers

### 1. Rename `pid_INC` → `pid_VRIN`? **No — keep `INC`.**

Three independent grounds:

**(a) The book is not a reliable naming authority even about itself.** I
verified directly in the EPUB that the Appendix captions call *both*
inconsistency tables VRIN — Table A–1: "…Self-Report Form Variable Response
Inconsistency (VRIN) scale score percentiles" and Table A–2: "…(100-item form)
Variable Response Inconsistency (VRIN) scale score percentiles" — while
Chapter 4's text calls the 100-item scale "PID-5-INC-S" (6 occurrences,
including "the PID-5-INC-S total score can range from 0 to 30"). So the book
uses VRIN and INC-S for the *same* 100-item scale depending on which page you
read. Harmonizing an exported API to a source that cannot keep its own naming
straight imports that instability into the package.

**(b) The primary sources support `INC`.** Keeley et al. (2016) is titled
"Development of a **Response Inconsistency Scale** for the Personality
Inventory for DSM-5"; Lowmaster et al. (2020) is titled "Further Validation of
the **Response Inconsistency Scale** for the PID-5" and names its short form
**PID-5-INC-S** — an abbreviation whose very structure (`INC` + `-S`) treats
`INC` as the parent scale's stem. Bagby & Sellbom (2018), already cited in
`R/validity_pid5.R`, is titled "The Validity and Clinical Utility of the
Personality Inventory for DSM-5 **Response Inconsistency Scale**". The
literature has two naming lineages (the Fossati/Somma line, two of the book's
four authors, writes PID-5-VRIN; the Keeley/Lowmaster/Bagby–Sellbom line
writes Response Inconsistency Scale / INC-S). The package sits in the lineage
of the papers it actually keys from, which is the right place to sit.

**(c) Cost/benefit under GP3.** The rename is an irreversible break touching
the exported output column, the `pid_items` keying columns (an IP1 surface),
the `@details` cut-score documentation, and ~70 literal occurrences across
code and tests — to match one secondary source that half-agrees with itself.
The user harm it would prevent (a reader of the book looking for `pid_VRIN`)
is fully addressed by a one-line documentation note (Q4). Researcher-first
ergonomics favors name stability plus a documented alias over churn.

### 2. What about `INCS` if (1) were yes? **Harmonization only works as a set — and no consistent set exists.**

A `VRIN`/`INCS` pair severs the lineage the current names encode (the SF scale
is the FULL scale's short form, built from 10 of Keeley's 20 pairs — the tests
in `test-keying.R` even assert every INC-S pair *is* a Keeley INC pair). A
partial rename is therefore not acceptable. But the completed set is also
unreachable: renaming `INCS` → `VRINS` would contradict both Lowmaster's own
published name (PID-5-INC-S) and the book's Chapter 4 text, while keeping
`INCS` contradicts the book's A–2 caption. That there is *no* rename of the
pair consistent with all of the book, the development papers, and internal
lineage is itself a strong argument for the answer to Q1: the current
`INC`/`INCS` pair is the only naming that is simultaneously
internally-consistent and primary-source-consistent.

### 3. Is the book the more authoritative source for naming? **No — it is authoritative for its norms, secondary for others' scale names.**

The distinction that resolves the brief's "not obvious which this book is"
question: authority is per-content-type, not per-document. For the **normative
tables**, the book is the primary source — the percentiles originate there,
computed on the book's normative sample — and M25 correctly cites it as the
authority (IP2/IP3). For the **identity and name of a scale the book did not
develop**, the development paper is the primary source and the book is a
secondary description, regardless of its authors' standing as PID-5
originators (Markon and Krueger authored the *instrument*; Keeley, Lowmaster,
Sellbom, and Williams authored the *validity scales*). The repo's standing
rule — primary beats secondary — therefore points at Keeley/Lowmaster for
naming, and their titles say Response Inconsistency Scale. One document can be
primary for one table and secondary for the label at the top of it; that is
exactly the situation here.

### 4. Is there a dominating third option? **Yes: keep the names, document the alias — split across two homes.**

Keep `INC`/`INCS` and ship the book-name → package-column mapping as
documentation, in two places serving two audiences:

- **`cairn/SOURCES.md` (tracking; required anyway).** AC2 already mandates the
  maintainer-confirmed mapping with page/table anchors. This is the
  provenance-grade record: verbatim book caption names, the A–1/A–2 caption
  inconsistency, per-table anchors. It belongs here because it is *evidence
  about sources*, and it must exist regardless of any package-facing surface.
- **Package documentation (user-facing; one sentence, not a dataset).** A
  reader of Markon et al. (2024) will look for "VRIN". The right surface is a
  brief "also known as" note: in the `pid_norms` roxygen block (in M25 scope —
  T8 writes that block anyway) and, as later work, in `validity_pid5()`'s
  `@details` (e.g. "the INC is also called the PID-5 Variable Response
  Inconsistency (VRIN) scale, e.g. by Markon et al., 2024"). M26's
  `norm_pid5()` help is the natural second home when it exists.

An **exported alias table** (a data object mapping book names to columns) is
rejected: it is registry elegance serving no scoring workflow (GP3), adds an
exported surface to maintain forever for what two doc sentences accomplish,
and invites the false impression that the package endorses multiple naming
schemes. If a future lookup function ever wants to accept "VRIN" as user
input, an internal `c(VRIN = "INC", ...)` vector at that point costs nothing
and needs no export today.

### 5. Deprecation path if renamed anyway. **Plain rename + NEWS entry; no dual columns, no lifecycle.**

Should the maintainer overrule Q1, the fitting path for this package is a hard
rename with a prominent NEWS entry: GP2 explicitly licenses it ("signatures
may break freely with NEWS, numbers never change quietly" — a column rename
changes no number), 0.2.0 already set the precedent of a deliberate breaking
round, and the tracking rules allow waiving the deprecation cycle pre-1.0 with
maintainer approval. Emitting both columns for a cycle is the worst option: it
doubles the validity surface, breaks the clean "one column per scale"
contract, and risks downstream code half-migrating. `lifecycle::deprecate_warn()`
is designed for function/argument deprecation, not output-column names (there
is no call site to warn at beyond the whole function), and would add an Import
for a single message against GP4 — `lifecycle` is not currently in
`DESCRIPTION` Imports. If renamed: one release, all surfaces at once (output
columns, `pid_items` columns, docs, tests), NEWS + D-entry.

### 6. Do the three surfaces differ? **No — all three should carry the package names, and they should stay identical to each other.**

- **(a) Scored-output columns:** keep `INC`/`INCS` etc. (Q1).
- **(b) Keying-table columns (`pid_items`, `data-raw/pid_items.csv`):** keep as
  is. These are doubly protected: they are IP1 content-adjacent, and the code
  *derives output names from them* (`inc_col <- paste0(prefix, inc_var)` where
  `inc_var` is literally the `pid_items` column name, `R/validity_pid5.R`
  lines 115/135, and siblings at 151/154, 170/173, 189/192). This coupling is
  a feature: it guarantees output and keying names can never drift. Any future
  rename must therefore change (a) and (b) together — another reason renames
  are expensive and another reason not to make this one.
- **(c) `pid_norms$scale`:** ratify the session's provisional decision —
  carry **package column stems** (`INC`, `INCS`, `ORS`, `PRD`), not book
  names. M26's lookup function then joins scored output to norm rows by
  string equality with zero crosswalk code, which is the researcher-workflow
  test GP3 imposes. Nothing is lost: the book's names live in SOURCES.md
  (AC2) and the `pid_norms` documentation note (Q4). Putting book names in
  the data and package names in the code would manufacture a permanent
  translation layer to honor the less consistent of the two sources.

### 7. Anything to change in how M25 records the mapping? **Two refinements to what AC2's record should say; no AC wording change needed.**

AC2's parenthetical "(VRIN, ORS, PIM-RD)" is close but the record itself
should use the book's *actual* labels, verified from the EPUB:

1. **The book's abbreviation for the Williams scale is PID-5-PRD, not
   PIM-RD.** Chapter 4 uses "PID-5-PRD" 15 times and "PIM-RD" zero times; the
   A–4 caption spells out "Positive Impression Management Response Distortion
   scale (underreporting)". So the PRD mapping is an identity
   (book PID-5-PRD → package `PRD`), which the record should state — it makes
   the INC/VRIN case the *only* genuine divergence.
2. **Record the book's internal inconsistency explicitly for A–2.** The
   SOURCES.md entry should quote both the A–2 caption ("Variable Response
   Inconsistency (VRIN)") and the Chapter 4 name ("PID-5-INC-S") with anchors,
   and state that the package maps A–2 to `INCS` on the strength of the
   chapter text, Lowmaster et al. (2020/2021), and the item-pair content —
   so a future reader auditing provenance is not stopped by the caption.

Additionally worth one line in the record: Chapter 4 contains no mention of
the Williams SD-TD scale at all (zero occurrences of "SD-TD", "Total Denial",
or "Social Desirability" as a scale name), and no appendix table norms it —
consistent with M25 shipping no SDTD table and creating no naming pressure on
`SDTD`/`SDTDS`.

## Beyond the brief

- **B1 — Who coined "INC" is unverified.** The Keeley et al. (2016) PDF is not
  on the source shelf, so I could not confirm what abbreviation Keeley's *text*
  uses (the title says "Response Inconsistency Scale"; the book claims Keeley
  "proposed … the PID-5 Variable Response Inconsistency (VRIN) validity
  scale", which — if the book is quoting rather than renaming — would mean
  even Keeley's in-text abbreviation was VRIN and `INC` descends from
  Lowmaster's PID-5-INC-S stem and Bagby & Sellbom's usage). This does not
  change any recommendation (stability, lineage, and Lowmaster's INC-S carry
  the argument regardless), but SOURCES.md should not assert that `INC` is
  "Keeley's abbreviation" without a maintainer glance at the paper's text.
- **B2 — Term-usage counts from the EPUB** (Chapter 4, for the record):
  PID-5-VRIN/VRIN ≈ 24 occurrences, PID-5-INC-S/INC-S 6, PID-5-ORS 24,
  PID-5-PRD 15, SD-TD/Total Denial 0. Appendix captions: A–1 "VRIN",
  A–2 "VRIN" (caption) vs chapter "PID-5-INC-S", A–3 "Over-Reporting Scale",
  A–4 "Positive Impression Management Response Distortion scale
  (underreporting)".
- **B3 — The name-derivation coupling is worth preserving deliberately.**
  Output column names being computed from `pid_items` column names (Q6b) is
  currently an implementation accident that functions as a consistency
  guarantee. If it is ever refactored away, a test asserting the
  output-name/keying-name correspondence should replace it.

## Recommendations

1. **Apply — keep all current validity-scale names** (`INC`, `INCS`, `ORS`,
   `ORSS`, `PRD`, `PRDS`, `SDTD`, `SDTDS`, `PNA`) on every surface: scored
   output, `pid_items` keying columns, and `pid_norms$scale`. Maintainer
   sign-off on this RR closes the question; no rename milestone is needed.
2. **Apply — ratify `pid_norms$scale` = package column stems** (`INC`,
   `INCS`, `ORS`, `PRD` for the four validity tables), per Q6c.
3. **Apply — within M25's existing AC2/T5 scope,** record the mapping in
   SOURCES.md using the book's verbatim labels and anchors, including the
   A–2 caption-vs-text inconsistency and the fact that the book's PRD
   abbreviation is PID-5-PRD (identity mapping), per Q7; and include a
   one-sentence book-name note in the `pid_norms` roxygen block T8 writes.
4. **Consider — future trivial-tier or M26 work:** add the "also known as
   VRIN (Markon et al., 2024)" sentence to `validity_pid5()`'s `@details`,
   and to `norm_pid5()`'s help when M26 creates it, so book readers find the
   columns.
5. **Consider — maintainer spot-check** of Keeley et al. (2016)'s in-text
   abbreviation next time the PDF is at hand (B1); record the answer in
   SOURCES.md.
6. **Reject — renaming `INC` to `VRIN`** (Q1: book internally inconsistent,
   primary sources favor INC, no consistent rename set exists, cost exceeds
   benefit).
7. **Reject — an exported alias dataset** (Q4: two documentation sentences
   dominate it; an internal lookup vector can be added later if a function
   ever accepts book names as input).

## Binding criteria

The naming outcome is "no change", but the brief flagged `pid_norms$scale` as
provisional and this RR is its ratification vehicle, so the following bind
M25's implementation. No numeric projections are made; no tolerances apply.

- **BC1.** In the shipped `pid_norms`, the `scale` values for the four
  validity tables are exactly the package column stems — `"INC"` (book Table
  A–1), `"INCS"` (A–2), `"ORS"` (A–3), `"PRD"` (A–4) — and no `scale` value
  anywhere in `pid_norms` is `"VRIN"`, `"VRINS"`, `"INC-S"`, `"PIM-RD"`, or
  any other book-caption spelling.
- **BC2.** M25 introduces no rename of existing validity-scale names on any
  exported surface: `validity_pid5()` output column names and the
  `pid_items`/`data-raw/pid_items.csv` validity columns (`INC`, `INCS`,
  `ORS`, `ORSS`, `PRD`, `PRDS`, `SDTD`, `SDTDS`) are unchanged from their
  pre-M25 state.
- **BC3.** The `cairn/SOURCES.md` PID-5 norms section required by AC2 records,
  with table/page anchors: (a) the A–2 caption name "Variable Response
  Inconsistency (VRIN)" *and* the Chapter 4 name "PID-5-INC-S" as an internal
  inconsistency of the book, with the mapping to `INCS` justified from the
  chapter text and Lowmaster et al. (2020/2021); and (b) the book's
  abbreviation PID-5-PRD (not "PIM-RD") mapped to package `PRD`.
