# RB04: Which document is the naming authority for a HiTOP-SR scale, and does it license renaming `NSSI`? (M058)

- **Date:** 2026-08-27
- **Output required:** write findings to `cairn/reviews/RR04-nssi-scale-name-authority.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`hitop` is an R package (version 0.2.0, pre-1.0, GPL-3, maintainer Jeffrey
Girard) that scores and distributes questionnaire instruments of the HiTOP
Society. One of them is the **HiTOP-SR**, a 405-item self-report instrument
whose 405 items are keyed to 76 primary scales (and 17 subscales). The package
is the Society's own distribution channel for the instrument: the Society's
page at <https://www.hitop-system.org/hitop-self-report-measures> says "The
most up-to-date files are hosted on the hitop R package website", and the
package ships four built HiTOP-SR questionnaires (two Word page sizes, a
Qualtrics import `.txt`, a REDCap `.zip`) that carry each scale's name on a
scoring page.

**The discrepancy.** 75 of the 76 scale names in the package's keying table are
written out in full — `Social Aloofness`, `Anxious Worry`, `Angry Hostility`.
One is an abbreviation: `NSSI`. It is the only one. The abbreviation flows
outward mechanically: the keying table's `Scale` column is converted to a
lower-camel stem by `snakecase::to_any_case()`, that stem names the scored
output column, and the printed name appears on the four distributed
questionnaires. So today a user scoring HiTOP-SR data gets a column named
`hsr_nssi` beside 75 columns like `hsr_socialAloofness`, and the four
questionnaires print `NSSI` where they print every other scale's full name.

**What milestone M058 proposes.** Rename the scale to its full name in the
keying source `data-raw/hitopsr_items.csv`, regenerate the four derived tables,
rebuild the four distributed artifacts and their `pkgdown/` copies, and accept
the consequent rename of the scored output column `hsr_nssi` (and its
`hsr_nssi_se` sibling) to whatever the new stem is. The milestone's Scope
places a **gate condition** ahead of all of this: it does not proceed unless a
citable source printing the scale's full name can be obtained, because the
package's inviolable principle IP1 puts keying-table content behind maintainer
sign-off *against an authoritative source*, and requires an unsourceable
discrepancy to be recorded as a visible open question instead of patched.

**Why this needs independent review.** A source was found (below), and the
milestone's own task list tags this decision as touching an inviolable
principle. The maintainer chose to escalate rather than adopt the name in the
implementation session. The question is not whether the abbreviation is ugly —
it is whether the document found is the kind of authority IP1 accepts for
instrument content, given that this repo has already decided one closely
analogous naming question (D-018) and one closely analogous
source-hierarchy question (D-032), and given that a *published* source covering
the same 76 scales is known to exist but is not yet in the maintainer's hands.

## Materials

Everything below is in the repository at the branch `m058-nssi-scale-name`.

**The candidate source (on the gitignored shelf, present locally):**

- `cairn/references/sources/simms2023_hitop_measurement_workgroup.pdf` — the
  HiTOP Measures Development Workgroup presentation deck, titled *Measurement
  Workgroup Presentation December 2023*, PDF author `Simms, Leonard`
  (Leonard J. Simms is the HiTOP-SR's lead developer and the chair of the
  measure-development effort), PDF CreationDate 2023-12-01, sha256
  `903e277334924d42286bbf261857ddea539b87e00eff5563784686711614681a`. Retrieved
  2026-08-27 from
  <https://ubwp.buffalo.edu/3plab/wp-content/uploads/sites/251/2026/07/MeasurementWorkgroupPresentationDecember2023.pdf>,
  the University at Buffalo 3P Lab page (<https://ubwp.buffalo.edu/3plab/hitop/>)
  that also serves `HiTOP-SR-Final.xlsx`, and which the Society's own measures
  page links as the introduction to the research-ready measure.
  **Slide 34** carries a table headed `HiTOP-SR Scale | # Items | alpha` under
  the section heading `Internalizing : Distress Scales`. Its rows include:

  ```
  Non-suicidal Self-Injury (NSSI)     6       0.83
  ```

  Extract it yourself with `pdftotext -layout` and read the surrounding slides;
  do not take this brief's transcription as the source. The string is plain
  ASCII: `Non-suicidal Self-Injury` — ordinary hyphens, capital `I` in
  `Injury`.

  Corroboration already performed: of the package's 76 `hitopsr_items$Scale`
  values, 75 appear as verbatim lines in the deck's text extraction and `NSSI`
  is the only one that does not; the deck's item count for the scale (6)
  equals the package's NSSI item count. Re-run this if you want it.

**The competing/silent source (on the shelf, present locally):**

- `cairn/references/sources/HiTOP-SR-Final.xlsx` — the HiTOP-SR development
  workbook, served from the same 3P Lab page and linked by the Society's page
  under "Legacy Development Files". It is the document this package's HiTOP-SR
  keying was built from. Five of its sheets mention the scale; every one of
  them writes `NSSI` and none writes a full name anywhere in the file.

**A known published source that is NOT in hand:**

- The **HiTOP-SR introduction paper**. Milestone `cairn/milestones/M041-hitopsr-score-intervals.md`
  (status: planned) is built around its **Table 1**, which prints, per
  HiTOP-SR primary scale and subscale, the item count, Cronbach's alpha, mean,
  and SD — the same three quantities the deck's slide-34 table prints, which
  suggests the deck's table is an earlier rendering of the same content.
  `cairn/DECISIONS.md` D-032 records (2026-08-13) that only a tracked-changes
  draft was observed and that "the final submitted version is not yet in hand";
  M041's task T1 is to ingest the final paper. It is not on the shelf and this
  review cannot read it. M041's acceptance criteria also record that **four of
  Table 1's labels do not join the package's scale names, one of them
  unresolvable** — so the paper's labels are known not to be identical to the
  package's, though which four is not recorded in this repo.

**The package's own files:**

- `data-raw/hitopsr_items.csv` — the keying source. Header:
  `HSR,Reverse,Spectrum,Subfactor,Scale,Subscale,Text,Original`. The six NSSI
  rows are lines 47, 216, 236, 299, 388, 405 (HSR item numbers 46, 215, 235,
  298, 387, 404), all `Internalizing / Distress / NSSI`.
- `data-raw/hitopsr_info.R` lines 1–90 — builds `hitopsr_items`,
  `hitopsr_scales`, `hitopsr_subscales`, `hitopsr_definitions` from that CSV.
  Line 18 derives `camelCase = snakecase::to_any_case(Scale, case = "lower_camel")`;
  that stem is what names the scored output column. Lines 47–55 carry a comment
  and the map `definition_scale_labels <- c("Non-suicidal Self-injury" = "NSSI")`,
  which exists solely because the in-repo definitions CSV writes the scale out
  where the keying table abbreviates it. Lines 74–86 are a `stopifnot` holding
  the definition stems in step with the scale and subscale stems.
- `data-raw/hitopsr_definitions.csv` line 59 — an **in-repo authored** table of
  clinician- and client-facing scale definitions (added by milestone M057),
  whose `Scale` cell reads `Non-suicidal Self-injury` — lowercase `i` in
  `injury`, differing from the deck. This file is not a source; it was written
  in this repository.
- `cairn/SOURCES.md` — the keying-provenance file. Note that it has a detailed
  provenance section for the PID-5 and for the HiTOP-HSUM, and for the
  HiTOP-SR only a punctuation note; the HiTOP-SR's scale names have no
  provenance line today.
- The four built artifacts that would be rebuilt: `inst/extdata/hitopsr_A4.docx`,
  `hitopsr_US.docx`, `hitopsr_qualtrics.txt`, `hitopsr_redcap.zip`, each with a
  byte-identical copy under `pkgdown/assets/downloads/`.
- `cairn/milestones/M058-nssi-scale-name.md` — the milestone: goal, scope,
  seven acceptance criteria, nine tasks, and a work log whose 2026-08-27
  entries record the source search and the escalation.

**Governing principles and prior decisions — read these in full:**

- `cairn/DESIGN.md` lines 94–106: **IP1** (instrument content is sacrosanct;
  changes only with maintainer sign-off "against the authoritative source (APA
  key, cited publication, or Society sanction)"; discrepancies "remain visible
  as open questions (OQ-n), never silently patched"; "a change to
  participant-facing text is a sourced content change, not a style fix"),
  **IP2** (ground truth, never self-reference), **IP3** (no scoring without a
  key; no norms without published tables), **GP2** (scored output never changes
  silently; "signatures may break freely with NEWS, numbers never change
  quietly"), **GP3**, **GP4**.
- `cairn/DECISIONS.md` **D-018** (2026-07-30) — the closest precedent. It
  declined to rename the PID-5 validity scale `INC` to the book's `VRIN`, and
  states the general rule it settles: *"authority is per-content-type, not
  per-document"* — the book is primary for its own normative tables, but for
  the identity and name of a scale it did not develop, the development paper is
  primary and the book is a secondary description, "regardless of its authors
  having created the instrument itself". It also records the migration path a
  future overruling would take: "one release, all surfaces at once, NEWS entry,
  no dual columns and no `lifecycle`".
- `cairn/DECISIONS.md` **D-032** (2026-08-13) — the closest source-hierarchy
  precedent. It holds that a peer-reviewed instrument paper's descriptive table
  clears IP3's published-source bar *where a development workbook does not*,
  and narrows an earlier maintainer correction that the HiTOP-SR development
  workbooks "are not trustworthy for clinical norming".
- `cairn/DECISIONS.md` **D-016** (b) — every artifact rebuild must add a
  `hitop_artifacts` manifest row or the checksum-lock test fails.

## Questions

1. **Does the December 2023 Measurement Workgroup deck clear IP1's bar as an
   authoritative source for a HiTOP-SR scale's name?** IP1 names three
   acceptable kinds — "APA key, cited publication, or Society sanction". The
   deck is not peer-reviewed and is not a publication in the citable sense; it
   is a slide presentation authored by the instrument's lead developer,
   distributed by the developing lab, and pointed to by the Society as the
   introduction to the measure. Argue whether that constitutes Society sanction
   for the *name of a scale*, and say what would change your answer.

2. **Applying D-018's "authority is per-content-type, not per-document" rule to
   this instrument, which document is primary for a HiTOP-SR scale's name?**
   The candidates are the development workbook (`HiTOP-SR-Final.xlsx`, which
   the package's keying was built from and which writes only `NSSI`), the
   Workgroup deck (which writes the full name and the abbreviation together),
   and the not-yet-in-hand introduction paper. Note the asymmetry to weigh: the
   workbook is the source the *keying* came from, so under a per-content-type
   reading it is arguably primary for *item-to-scale membership* without being
   primary for *what the scale is called*.

3. **Is `NSSI` in the workbook a competing name or an absence of one?** The
   implementation session read the workbook's `NSSI` as an abbreviation used in
   a working spreadsheet rather than a naming claim, and therefore found no
   source disagreement. Test that reading. If instead the two documents
   disagree, D-018's precedent (decline the rename, document the alternative)
   becomes the live option rather than an alternative.

4. **Should this milestone proceed now, or wait for the introduction paper?**
   The paper is a stronger source under D-032's ordering and covers the same 76
   scales with the same statistics, so its Table 1 almost certainly prints this
   scale's name. The cost of waiting is that the sole abbreviated scale name
   keeps shipping on four distributed questionnaires and in scored output for
   an unbounded time (M041 is planned, undated, and blocked on a document not
   in hand). The cost of not waiting is a second rename of a scored output
   column if the paper's spelling differs from the deck's — and this repo
   already knows four of Table 1's labels do not join the package's scale
   names. Weigh those, and say whether the answer changes if the maintainer can
   obtain the paper within this milestone.

5. **If the rename proceeds, exactly what string should be written into
   `data-raw/hitopsr_items.csv`?** The deck prints `Non-suicidal Self-Injury`;
   the in-repo definitions CSV writes `Non-suicidal Self-injury`; the wider
   literature commonly writes `Nonsuicidal Self-Injury` with no hyphen. The
   milestone's AC1 requires the written string be compared character for
   character against the source document read at verification time. Should the
   parenthetical `(NSSI)` be carried into the name, dropped, or preserved
   somewhere else? Consider that all three hyphenations and both casings
   convert to the same lower-camel stem `nonSuicidalSelfInjury`, so this choice
   changes only what is *printed* on the four participant-facing questionnaires
   and stored in the keying table — which IP1 calls "a sourced content change,
   not a style fix".

6. **Is the scored-output rename `hsr_nssi` → `hsr_nonSuicidalSelfInjury`
   proportionate, and should the stem be derived or chosen?** The package
   derives every scale's stem mechanically from its printed name, and the
   milestone's Scope keeps that derivation. The result is a 21-character stem
   where the other 75 average far less. The alternative — a name-to-stem
   exception keeping `hsr_nssi` while the printed name changes — would break
   the derivation the package relies on everywhere and reintroduce, in code, the
   very special case that `definition_scale_labels` exists to work around.
   GP2's pre-1.0 posture licenses the break with a NEWS entry and no
   `lifecycle` shim (which D-018 confirms is the path for an output-column
   rename). Say whether you agree, and whether the exception deserves
   consideration.

7. **If your answer to Q1 or Q4 is that the rename should not proceed now, what
   exactly should the package do instead?** IP1 requires an unsourceable
   discrepancy to be "visible as an open question (OQ-n), never silently
   patched", and `cairn/SOURCES.md` is where this repo's open questions live
   (see its OQ-1 and OQ-2 for the established shape). Specify where the open
   question should be recorded, what it should say, and what evidence would
   close it.

8. **Beyond the brief:** anything you find in the materials that bears on this
   decision and that the questions above miss — including anything wrong in
   this brief's own reasoning or transcriptions.

## Constraints

Fixed; flag disagreement explicitly rather than working around it.

- **IP1, IP2, IP3, GP2** as written in `cairn/DESIGN.md` are not up for
  relitigation. If your recommendation requires bending one, say so plainly and
  name the principle.
- **D-018** and **D-032** stand unless a recommendation explicitly supersedes
  them. This repo's convention is that a decision is superseded by an appended
  entry quoting the prior rationale, never silently overridden — so if you
  think either should give way here, say what the superseding entry must say.
- **The milestone's Out-of-scope list holds:** no `lifecycle` deprecation shim
  for the old column name; no rename of any other scale on this or any other
  instrument; no change to milestone M057's tooltip work, which joins on the
  stem and is correct either way.
- **The decision is the maintainer's.** Your report is advisory. Do not edit any
  file other than the output path above.
- **Do not run `data-raw/hitopsr_info.R`, rebuild any artifact, or modify any
  file under `data/`, `inst/extdata/`, or `pkgdown/`.** Reading is expected;
  writing is not.

## Output format

In `RR04-nssi-scale-name-authority.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under "Beyond
the brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason. Your report is advisory: emit a `## Binding criteria`
section ONLY if this brief's header slot says `requested`.
