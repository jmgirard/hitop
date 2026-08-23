# RB01: Validity-scale naming — harmonize to the book, or keep primary-source names? (M025)

- **Date:** 2026-07-30
- **Output required:** write findings to `cairn/reviews/RR01-validity-scale-naming.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`hitop` is an R package (version 0.2.0, pre-1.0, GPL-3, maintainer Jeffrey
Girard) that scores and distributes HiTOP Society questionnaire instruments.
One of those instruments is the Personality Inventory for DSM-5 (PID-5), which
the package scores in three versions: FULL (220 items), SF (100 items), and BF
(25 items).

Alongside trait/domain scores, the package computes **validity scales** —
indices that flag probably-invalid response protocols. `validity_pid5()`
returns them as columns named with a user-settable prefix (default `pid_`) plus
a scale abbreviation, e.g. `pid_INC`, `pid_ORS`, `pid_PRD`. The abbreviations
currently shipped are:

| Column | Scale | Applies to | Source the package cites |
|---|---|---|---|
| `INC` | Response Inconsistency Scale | FULL | Keeley et al. (2016) |
| `INCS` | Response Inconsistency Scale — Short Form | SF | Lowmaster et al. (2020) + Correction (2021) |
| `ORS` | Over-Reporting Scale | FULL | Sellbom, Dhillon & Bagby (2018) |
| `ORSS` | Over-Reporting Scale — Short Form | SF | (short-form variant; cut-scores unvalidated) |
| `PRD` | Positive Impression Management Response Distortion | FULL | Williams et al. (2019) |
| `PRDS` | PRD — Short Form | SF | (short-form variant; cut-scores unvalidated) |
| `SDTD` | Social Desirability / Total Denial | FULL | Williams et al. (2019) |
| `SDTDS` | SD-TD — Short Form | SF | (short-form variant; cut-scores unvalidated) |
| `PNA` | Percent missing (not a published scale) | all | — |

**What prompted this review.** Milestone M025 is ingesting the PID-5 normative
tables from a newly acquired primary source:

> Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
> *Understanding the Personality Inventory for DSM-5 (PID-5).*
> American Psychiatric Association Publishing. ISBN 9781615375127.

The book's Appendix carries 12 normative tables; M025 ships seven of them.
Four of those seven are validity-scale percentile tables, and while mapping
them to package columns it emerged that the book's naming and the package's
naming diverge on exactly one scale — and that **the book is internally
inconsistent about it**.

The book's own text (Chapter 4, "PID-5 Response Validity Assessment and
Validity Scales") says, quoting the relevant passages:

- On the FULL-form inconsistency scale: "Keeley and colleagues (2016) proposed
  a scale to detect inconsistency in PID-5 responses, the PID-5 Variable
  Response Inconsistency (**VRIN**) validity scale. The procedure in developing
  this scale was similar to that used in developing the Minnesota Multiphasic
  Personality Inventory (MMPI) VRIN scale."
- On the SF-form inconsistency scale: "Lowmaster et al. (2020) developed a
  response inconsistency scale for the 100-item version of the PID-5 …
  Building on Keeley et al.'s (2016) work … Lowmaster et al. (2020) developed
  and cross-validated the PID-5-SF Response Inconsistency Scale
  (**PID-5-INC-S**)."
- On overreporting: "Sellbom et al. (2018) developed a scale to detect
  overreporting on the PID-5. The PID-5 Over-Reporting Scale (**PID-5-ORS**)".
- On underreporting: "Williams and colleagues (2019) developed the PID-5
  Positive Impression Management Response Distortion (**PID-5-PRD**) scale".

So the book agrees with the package on ORS, PRD, and INC-S, and differs only on
Keeley's FULL-form scale, which it calls VRIN where the package calls it INC.
Note that Keeley et al.'s own paper is titled "Development of a **Response
Inconsistency Scale** for the Personality Inventory for DSM-5", and Lowmaster
et al.'s is titled "Further Validation of the **Response Inconsistency Scale**
for the PID-5" — neither original paper's title uses "VRIN".

The maintainer asked whether the package should harmonize its naming to the
book. That is an irreversible exported-API decision (renaming output columns
that user code indexes by name), which is why it is being escalated rather than
settled in-session.

## Materials

Read these, in this order:

1. `R/validity_pid5.R` — the exported function. Note especially:
   - lines 13–29: the `@details` block stating cut-scores by scale abbreviation.
   - line 110: `names(out) <- paste0(prefix, "PNA")`.
   - lines 114–115, 150–151, 169–170, 189–192: where each scale's column name
     is chosen, including the FULL-vs-SF branch
     (`inc_var <- ifelse(version == "FULL", "INC", "INCS")` and its siblings).
   - line 169's comment already reads "Positive Impression Management Response
     Distortion Scale (PRD)" — i.e. the package already uses the book's full
     scale name for PRD.
2. `R/data.R` — documentation for `pid_items`, whose columns `INC`, `INCS`,
   `ORS`, `ORSS`, `PRD`, `PRDS`, `SDTD`, `SDTDS` carry validity-scale
   membership. These are the keying table's column names, not just output
   names.
3. `data-raw/pid_items.csv` — the keying table itself (same column names).
4. `tests/testthat/test-validity_pid5.R`, `tests/testthat/test-keying.R`,
   `tests/testthat/helper-fixtures.R` — the test surface. Occurrence counts of
   the literal string `INC` (which also matches `INCS`): `R/validity_pid5.R` 7,
   `R/data.R` 2, `test-validity_pid5.R` 21, `test-keying.R` 23,
   `helper-fixtures.R` 20, `data-raw/pid_items.csv` 2.
5. `cairn/SOURCES.md` — the repo's keying-provenance file. Its "Sources"
   section lists the primary source the package cites for each validity scale.
6. `cairn/DESIGN.md` lines 88–98 — the numbered principles IP1–IP4 and GP1–GP4.
   IP2, GP2, and GP3 are the ones that bear on this question; they are quoted
   under Constraints below.
7. `cairn/milestones/M025-pid5-norms-ingest.md` — the milestone in flight, for
   its Goal and Scope sections only.

The book itself is on the gitignored source shelf at
`cairn/references/sources/markon2024.epub`. It is a standard EPUB; its
appendix is `OEBPS/xhtml/20_Appendix.xhtml` and chapter 4 is
`OEBPS/xhtml/10_Chapter04.xhtml`, both well-formed XHTML readable with
`python3` + `zipfile`. No `cairn/references/<citekey>.md` summary exists yet —
authoring it is a later task of M025 (T5), so quote the book directly if you
need more than the passages excerpted above.

## Questions

1. Should the package rename its FULL-form inconsistency column from `INC` to
   `VRIN` to match the book? Answer for the **exported output column name**
   (`pid_INC` → `pid_VRIN`) specifically.
2. If the answer to (1) is yes, what should happen to `INCS`? The book calls
   that scale PID-5-INC-S, so harmonizing (1) alone produces a `VRIN`/`INCS`
   pair whose shared lineage is no longer visible in the names. Is that
   acceptable, or does harmonization only make sense as a set?
3. For scale *naming* specifically, is Markon et al. (2024) — written by two of
   the PID-5's original authors and published by APA Publishing — a more
   authoritative source than the individual scale-development papers (Keeley
   et al. 2016; Lowmaster et al. 2020)? The repo's standing rule is that
   primary sources beat secondary descriptions, but it is not obvious which
   this book is for a scale it did not develop.
4. Is there a third option that dominates both, e.g. keeping `INC` as the
   column name while shipping a documented book-name → column-name alias table
   that users and future lookup functions can consult? If so, where should
   that table live, and does it belong in the package (exported data) or in
   `cairn/SOURCES.md` (tracking only)?
5. If a rename is warranted, what deprecation path fits a pre-1.0 package whose
   GP2 states that "signatures may break freely with NEWS"? Specifically:
   should the old column name be emitted alongside the new one for a cycle,
   emitted with a `lifecycle::deprecate_warn()`, or simply changed with a NEWS
   entry?
6. Does the answer differ between the two surfaces? Namely (a) the
   **scored-output column names** from `validity_pid5()`, (b) the
   **keying-table column names** in `pid_items`/`data-raw/pid_items.csv`, and
   (c) the `scale` **values inside the forthcoming `pid_norms` dataset** that
   M025 is building. The session's provisional decision — not yet ratified — was
   that `pid_norms$scale` should carry package column names so that M026's
   planned lookup function can join scored output to norm rows by name.
7. Does anything in the review change how M025 should record the mapping in
   `cairn/SOURCES.md`? M025's acceptance criterion AC2 already requires "the
   maintainer-confirmed mapping from each book scale name (VRIN, ORS, PIM-RD)
   to the package column it norms".

## Constraints

Fixed; do not relitigate. Flag disagreement explicitly rather than working
around a constraint silently.

- **IP1 — instrument content is sacrosanct.** Keying tables, scale memberships,
  item text, response options, and administration instructions change only with
  maintainer sign-off against the authoritative source. This review may
  *recommend* a naming change but cannot authorize one; the maintainer signs
  off.
- **IP2 — ground truth, never self-reference.** Every shipped numeric constant
  that affects output traces to a `SOURCES.md`-cited authority before it ships.
- **IP3 — no scoring without a key; no norms without published tables.**
- **GP2 — scored output never changes silently.** "Signatures may break freely
  with NEWS, numbers never change quietly." A column rename is an interface
  break, not a change of numbers, so GP2 permits it with a NEWS entry — this
  bears directly on question 5.
- **GP3 — researcher-first ergonomics.** In API design conflicts, the applied
  researcher processing response data wins over registry elegance. IP1–IP4
  outrank convenience wherever they apply.
- **The package is version 0.2.0 (pre-1.0)** and has already shipped one round
  of deliberate breaking API changes (see `NEWS.md` for 0.2.0). The
  tracking rules allow a pre-1.0 project to waive the deprecation cycle with
  explicit maintainer approval.
- **M025's scope is fixed** and does not include renaming anything: it ingests
  the seven norm tables, writes provenance, builds `pid_norms`, and tests it.
  If you recommend a rename, recommend it as separate future work, not as an
  M025 amendment.
- **Do not modify any file** other than writing your findings to the RR path.
  In particular, do not edit `R/`, `data-raw/`, `tests/`, or the milestone file.

## Output format

In `RR01-validity-scale-naming.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under "Beyond
the brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason. Where findings bind implementation, also emit a
`## Binding criteria` section: numbered `BC1…`, each a measurable assertion
checkable against evidence, with any numeric projection stating its tolerance.
These are ingested VERBATIM into the constrained milestone's acceptance
criteria and mechanically diffed against this file; departures are legal only
through that milestone's shown "Deviations from RR01" table.

Note: if your conclusion is that no implementation change should follow (for
example, that the package should keep its current names), then emit **no**
`## Binding criteria` section at all rather than an empty one — M025's
acceptance criteria are then left exactly as planned.
