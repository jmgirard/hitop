# RR04: Which document is the naming authority for a HiTOP-SR scale, and does it license renaming `NSSI`? (M058)

- **Date:** 2026-08-27
- **In response to:** `cairn/reviews/RB04-nssi-scale-name-authority.md`
- **Advisory only** (binding criteria not requested)

Evidence in this report was re-derived from the materials, not taken from the
brief: the deck's sha256 and metadata were re-checked (`903e2773…4681a`,
Author `Simms, Leonard`, CreationDate 2023-12-01), the slide-34 table was
re-extracted with `pdftotext -layout` (`Non-suicidal Self-Injury (NSSI)  6  0.83`
under `Internalizing : Distress Scales`), the 75-of-76 verbatim corroboration
was re-run programmatically against `data/hitopsr_items.rda` (75 matched, `NSSI`
the sole miss; 6 NSSI items), the workbook's five NSSI-bearing sheets were
re-read, the snakecase stem conversions were executed, and the Society page was
fetched (it links the deck as "Download introduction slideshow" and says "The
most up-to-date files are hosted on the hitop R package website").

## 1. Does the deck clear IP1's bar?

**Yes — under the Society-sanction prong, as the weakest admissible form of it,
and subordinate to the introduction paper once that is in hand.**

It is plainly not an APA key, and I would not stretch "cited publication" to
cover a slide deck: in this repo that phrase has consistently meant
peer-reviewed or formally issued documents (Keeley 2016, Lowmaster 2021
Correction, Markon 2024). The live prong is Society sanction, and the deck's
claim to it is a chain, each link verifiable:

- authored by Leonard Simms, the HiTOP-SR's lead developer and the
  Measures Development Workgroup chair — the person whose naming decisions
  *are* the instrument's naming decisions;
- distributed by the developing lab's page, the same page that serves
  `HiTOP-SR-Final.xlsx`, the document this package's keying was built from;
- linked from the Society's own measures page as the measure's
  "introduction slideshow" (verified 2026-08-27);
- describing the *final* instrument, not a draft: its counts (405 items, 76
  scales, 17 subscales) match the package exactly, 75 of the package's 76
  scale names appear verbatim, and the NSSI row's item count (6) matches the
  keying table.

That last point matters most. IP1's purpose is that instrument content trace to
the instrument's owner rather than to a package author's judgment. For the
narrow content-type "what the developer calls this scale," a presentation in
which the developer introduces the finished measure and prints all 76 names,
endorsed by the Society as the measure's introduction, is the developer speaking
in an official capacity. It is sanction, thinly formalized.

**What would change my answer:** the Society page delisting the deck; evidence
the deck predated final scale honing (item counts refute this); or — decisive —
the introduction paper arriving and printing a different string, since under
D-032's ordering and D-018's content-type rule the paper outranks the deck the
moment it exists in hand (see Q2). The deck's authority here is best-available,
not final.

## 2. Which document is primary under D-018's per-content-type rule?

**The introduction paper, once in hand. Until then, the deck. The workbook is
primary for membership, never for naming.**

D-018's rule: for the identity and name of a scale, the development paper is
primary. The HiTOP-SR's development paper is the introduction paper — not on
the shelf (D-032: "the final submitted version is not yet in hand"; M041 T1).
The brief's asymmetry is right and worth stating as the holding: the workbook
is the source the keying was *built from*, so it is primary for item-to-scale
membership, reverse flags, and item text — but a working spreadsheet's column
labels are not naming claims of the same rank (see Q3), so per-content-type
primacy for the *name* does not follow from keying primacy.

The deck sits between them: it is not the development paper, but it is the same
author presenting the same table (scale, item count, alpha) that the paper's
Table 1 prints — an earlier rendering of the primary source's own content, in
the primary author's voice. That gives it a standing the workbook lacks and the
paper exceeds. Ordering for this content type: **paper > deck > workbook**.

## 3. Is the workbook's `NSSI` a competing name or an absence?

**Not a competing name — but the implementation session's "silent" framing
needs one correction.** The workbook is not silent: I verified that in the
sheets where it writes `NSSI` (`HiTOP-SR items by scale`, `Descriptives
prolific final`, `alpha prolific raw`), the *other* scales appear under their
full names — `Anxious Worry`, `Angry Hostility`, `Social Aloofness`,
`Suicidality`. So `NSSI` occupies name-position in a document that otherwise
uses full names. Read alone, that could be a naming claim.

What defeats that reading is the deck itself: the same author prints
`Non-suicidal Self-Injury (NSSI)` — asserting, in one cell, that NSSI is the
abbreviation of a full name. The two documents therefore do not disagree about
what the scale is called; they differ in whether the writer expanded the
abbreviation. This is nothing like D-018's INC/VRIN situation, where two
distinct authorities used two distinct *names* and no rename could satisfy
both. Here one authority uses one name at two levels of contraction. D-018's
decline-and-document branch is not triggered by the workbook.

The only genuine inter-source discrepancy in the materials is a casing one the
brief did not surface — deck `Self-Injury` versus the draft paper's
`Self-injury` — covered under Q8, finding 1.

## 4. Proceed now, or wait for the introduction paper?

**Attempt to obtain the paper first; proceed on the deck only if that attempt
fails within a bounded window. If the maintainer can obtain the paper within
this milestone — and M041 T1 says the need is "the final version from Len,"
i.e. one request to a collaborator the workgroup chair — then wait, clearly.**

The cost accounting in the brief needs two corrections before it can be
weighed:

- The brief's stated risk of not waiting — "a second rename of a scored output
  column if the paper's spelling differs from the deck's" — is overstated for
  the spellings actually in evidence. Both documents in hand hyphenate, and
  `Non-suicidal Self-Injury` and `Non-suicidal Self-injury` produce the
  identical stem `nonSuicidalSelfInjury` (verified with
  `snakecase::to_any_case`). A casing mismatch with the final paper costs a
  DOCX rebuild and a SOURCES line, not a column rename. Only the unhyphenated
  literature form `Nonsuicidal Self-Injury` changes the stem (to
  `nonsuicidalSelfInjury` — see Q8, finding 2, correcting the brief's claim
  that all variants converge), and neither the deck nor the draft paper's
  Table 1 uses it.
- The repo *does* record which draft-paper labels fail to join (Q8, finding 1):
  one of the four is this very scale, printed `Non-suicidal Self-injury` in
  the 2026-03-24 draft. So the stronger source's draft is already known to
  disagree with the deck in casing. Proceeding on the deck's capital-I string
  therefore carries a *probable* (not merely possible) later one-character
  reconciliation against the final paper.

With those corrections: the cost of waiting is the abbreviation shipping for an
unbounded further time on the two DOCX questionnaires and in scored output; the
cost of proceeding now is a likely later DOCX rebuild (bounded, mechanical,
D-016 manifest machinery exists for exactly this) and a small tail risk of a
second column rename if the final paper dehyphenates. Neither cost is severe.
What tips it is that obtaining the paper is cheap, is *already a planned task*
(M041 T1), and collapses every risk at once: the D-entry then cites the primary
source, AC1's character-for-character check runs against the real authority,
and no reconciliation debt is created. Escalation already stopped the
milestone; spending that stop on one email to Len is the efficient move.

If the paper cannot be obtained within a window the maintainer sets (I would
suggest a few weeks, matching how long M041 has already idled), proceed on the
deck per Q1, with the reconciliation commitment written into the D-entry
(Recommendation R2).

## 5. If the rename proceeds from the deck, what string?

**`Non-suicidal Self-Injury` — the deck's characters exactly, with the
parenthetical `(NSSI)` dropped from the `Scale` cell.**

- The parenthetical is an abbreviation gloss, not part of the name: verified,
  no other scale row in the deck's tables carries one, and `NSSI` occurs
  exactly once in the whole deck — the slide-34 cell, introducing the
  abbreviation in the standard "Name (ABBR)" form. Carrying it into the
  keying table would print this one scale styled unlike the other 75 on the
  DOCX scoring pages — recreating the anomaly M058 exists to remove — and
  would put parentheses into the cell every stem derives from.
- AC1's character-for-character comparison must therefore be worded to compare
  the name span — the cell text before the abbreviation gloss — and the
  SOURCES.md line should state explicitly that `(NSSI)` is printed in the
  source and deliberately not carried. Otherwise AC1 fails on its own terms at
  verification time.
- `NSSI` should survive in two places only: an "also known as" sentence in the
  documentation (`R/data.R` / `?hitopsr_items`), which is D-018's established
  pattern for a name the literature will keep using, and the NEWS entry
  ("formerly `hsr_nssi`"). Not in any data cell.
- Do **not** adopt the definitions CSV's `Non-suicidal Self-injury` as the
  written string while the deck is the source: it is in-repo authoring, and
  writing it against a source that prints a capital `I` would fail AC1
  literally. (If the *paper* becomes the source and prints lowercase `i`, its
  string wins — Q4.) Whichever string is adopted, the banked gate choice —
  align `hitopsr_definitions.csv` and delete the then-inert
  `definition_scale_labels` map in `data-raw/hitopsr_info.R`, keeping the
  `stopifnot` — is correct and should be executed with it.

## 6. Is the derived stem proportionate — derived or chosen?

**Derived. Agree with the milestone's Scope; the exception deserves rejection,
not consideration.**

- The length worry is empirically hollow: the package already ships
  `hsr_difficultiesReachingOrgasm` (26-character stem) and
  `hsr_sexRelatedSubstanceUse` (22). `nonSuicidalSelfInjury` (21) is not even
  the longest; it lands inside the existing distribution.
- A name-to-stem exception keeping `hsr_nssi` would break the invariant that
  every consumer relies on — `hitopsr_scales$camelCase` names the
  `itemNumbers`, the scored columns, the definitions join, and M057's tooltip
  join all derive from the printed name — and would reintroduce *in shipped
  code* the special case that `definition_scale_labels` exists to paper over
  in a build script. The milestone's own goal line ("the picker, the forms and
  the scored output agree") is the argument.
- The abbreviation-column convention (`pid_PNA`, `pid_INC`) is for validity
  scales; NSSI is a content scale and takes the content-scale convention.
- GP2's pre-1.0 posture plus D-018's recorded migration path (one release, all
  surfaces at once, NEWS, no dual columns, no `lifecycle`) is exactly what
  M058 already specifies. No principle is bent.

## 7. If the rename does not proceed now, what instead?

Record **OQ-3** in `cairn/SOURCES.md` under "Open questions (need source
adjudication)", in OQ-1's shape:

- **Title:** "OQ-3: What is the full printed name of the HiTOP-SR `NSSI`
  scale?"
- **Body:** `hitopsr_items$Scale` abbreviates one scale of 76 (`NSSI`, 6 items:
  HSR 46, 215, 235, 298, 387, 404) where the other 75 carry full names. The
  development workbook (`HiTOP-SR-Final.xlsx`) writes only `NSSI` in all five
  sheets that mention it. The Measurement Workgroup deck (Simms, December
  2023; sha256 `903e2773…`; slide 34) prints `Non-suicidal Self-Injury
  (NSSI)`. The introduction paper's 2026-03-24 tracked-changes draft prints
  `Non-suicidal Self-injury` in Table 1 (observed 2026-08-13, M041; nothing
  ships from a draft, D-032). The two admissible-in-principle sources thus
  differ in casing.
- **Resolution:** the final submitted introduction paper's Table 1 string,
  ingested at M041 T1; adopting it executes M058.
- **Impact:** the printed name on the two DOCX questionnaires and the scored
  output column `hsr_nssi`/`hsr_nssi_se`.

Mirror it as a numbered row in `cairn/DESIGN.md` "Known issues & tech debt"
referencing OQ-3, which is OQ-1's established second surface. M058's status
stays `blocked` with a work-log line pointing at OQ-3. A `skip()`-ed test is
how OQ-1/OQ-2 stay visible in the suite, but those guard falsifiable keying
assertions; this OQ asserts nothing testable yet, so a skip is optional — if
added, it should merely name OQ-3 as the reason.

## Beyond the brief

1. **The brief is wrong that "which four \[Table 1 labels\] is not recorded in
   this repo."** `cairn/milestones/M041-hitopsr-score-intervals.md` T3 records
   all four: `Manic Energy†`, `Non-suicidal Self-injury`, `p-factor`,
   `Appearance Focus` — and the unresolvable one is `Appearance Focus`, not
   this scale. The decision-relevant consequence: the draft introduction paper
   is already observed to print this scale's full name as `Non-suicidal
   Self-injury`, lowercase `i`, disagreeing with the deck's casing. This is
   the single most material fact the brief missed; it drives my Q4 answer.
2. **The brief's Q5 stem claim is false for the unhyphenated form.** Verified:
   `snakecase::to_any_case("Nonsuicidal Self-Injury", case = "lower_camel")`
   yields `nonsuicidalSelfInjury`, not `nonSuicidalSelfInjury`. The two
   hyphenated casings do converge. So "this choice changes only what is
   printed" holds only across hyphenated variants; a dehyphenated final-paper
   spelling would force a second scored-column rename. Both in-hand documents
   hyphenate, so the tail risk is small but it is not zero as the brief
   asserts.
3. **Only the two DOCX artifacts print scale names.** Verified directly:
   `inst/extdata/hitopsr_qualtrics.txt` contains no scale-name strings at all
   (zero matches for `nssi` and for control names like `Aloofness`), and the
   REDCap zip's `instrument.csv` likewise carries only item text and
   `hsr_NNN` field names. The brief's "the four questionnaires print `NSSI`
   where they print every other scale's full name" is wrong for two of the
   four, and — more consequentially — **M058's AC4 as written is
   unsatisfiable**: "each of the four rebuilt artifacts carries the new name,
   read back by parsing the file" cannot be met by the Qualtrics or REDCap
   files, whose rebuilds may not even change bytes. AC4/T5 need rewording at
   the plan gate (e.g. DOCX: carries the new name; Qualtrics/REDCap: asserted
   unchanged, or asserted to contain neither old nor new name).
4. **The definitions CSV's spelling matches the draft paper's Table 1 label
   exactly** (`Non-suicidal Self-injury`). The gate's "in-repo authoring, not
   a source" ruling stands formally, but the coincidence suggests the CSV's
   author followed the draft — which raises the odds the *final* paper prints
   lowercase `injury` and the deck's casing loses a later reconciliation.
5. **"The workbook is silent" is not quite right** — it uses `NSSI` in
   name-position where other scales get full names (Q3). The conclusion (no
   competing name) survives, but the D-entry should carry the corrected
   framing, since "silent" is the load-bearing word in the implementation
   session's log.
6. **The deck's hosting URL is a July-2026 WordPress upload path**
   (`/2026/07/`) of a December-2023 file — currently served, but movable; the
   gitignored shelf copy plus the recorded sha256 is the durable citation, and
   the SOURCES.md line should carry both, as the HSUM entry's pattern does.

## Recommendations

- **R1 (apply):** Before adopting any string, request the final introduction
  paper (M041 T1's own need — "the final version from Len") with a bounded
  wait the maintainer sets. If it arrives, its Table 1 string is the source;
  the deck becomes corroborating provenance in the SOURCES.md line.
- **R2 (apply):** If the window closes empty, proceed on the deck under IP1's
  Society-sanction prong with `Non-suicidal Self-Injury` (no parenthetical),
  and write into the D-entry a reconciliation commitment: at M041 T1 the
  adopted string is diffed against the final Table 1; a casing-only difference
  triggers a DOCX rebuild + SOURCES amendment without re-litigation, and a
  stem-changing difference reopens the decision.
- **R3 (apply):** Keep the mechanically derived stem
  (`hsr_nonSuicidalSelfInjury`); reject the name-to-stem exception (Q6).
- **R4 (apply):** Reword AC4/T5 for the fact that only the DOCX artifacts
  print scale names (Beyond-the-brief 3), and word AC1's
  character-for-character check as comparing the name span before the
  `(NSSI)` gloss, with the gloss's omission stated in the SOURCES.md line.
- **R5 (apply):** With whichever string is adopted: `NSSI` survives as an
  also-known-as documentation sentence and in NEWS ("formerly `hsr_nssi`");
  execute the banked gate choice (align `hitopsr_definitions.csv`, delete
  `definition_scale_labels`, keep the `stopifnot`).
- **R6 (consider):** If neither paper nor decision lands within the window,
  record OQ-3 exactly as Q7 specifies and leave M058 blocked on it.
- **R7 (reject with reason):** Adopting the definitions CSV's `Non-suicidal
  Self-injury` now on the theory it matches the draft paper — the CSV is
  in-repo authoring and the draft is non-shippable scoping evidence (D-032);
  writing it into the keying table would be an unsourced content change, the
  precise thing IP1 forbids.
- **R8 (reject with reason):** Carrying `(NSSI)` into the `Scale` cell — it is
  an abbreviation gloss, would restyle this one scale against the other 75 on
  the printed forms, and would pollute the stem-deriving cell.
