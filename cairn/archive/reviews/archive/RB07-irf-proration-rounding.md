# RB07: Proration rounding for the PID-5 Informant Form (M159)

- **Date:** 2026-10-03
- **Output required:** write findings to `cairn/reviews/RR07-irf-proration-rounding.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`hitop` is an R package that scores HiTOP Society and related personality
questionnaires. Its `score_pid5()` scores the Personality Inventory for DSM-5
(PID-5) in several versions: FULL (220 items), SF (100), BF (25) and BFPM (36).
Milestone M159 adds a fifth version, `"IRF"`, the 218-item PID-5 Informant Form
(Markon, Quilty, Bagby & Krueger, 2013), published by the American Psychiatric
Association (APA) with its own scoring key. Decision D-089 already fixed the
version name, the storage and the reverse-keyed items.

Under the default `missing = "apa"`, every version applies the APA
missing-data rule through one helper, `apa_mean()`. A facet with more than 25%
of its items unanswered is `NA`. Otherwise the answered items are summed, the
partial sum is multiplied by the facet's item count and divided by the number
answered, and the result is rounded to a whole number before it is divided by
the item count to give the facet's average. The helper rounds half up
(`floor(x + 0.5)` on non-negative values). The rule came from the adult
self-report key's wording, "If the result is a fraction, round to the nearest
whole number" (decision D-009).

The informant key's instructions (PDF p. 9) say instead: "If the result is a
fraction, round **up** to the nearest whole number." The two APA child-form keys
on the package's source shelf say "round to the nearest whole number". The
informant key is otherwise a light edit of the self-report key's text, and it is
known to carry at least one copy error: its Step 1 reverse list names 16 items,
two of which (98, 176) its own Facet Table does not mark and whose wording is not
reversed. The self-report reverse list, renumbered, explains those two. D-089(c)
resolved that conflict for the Facet Table.

The milestone's implementer stopped at this point: the fixture test cannot be
written until the rounding rule is fixed, and the maintainer chose an
independent review over deciding directly. The implementer's tentative
recommendation was to follow the informant key as printed (round up), on the
grounds that the package's principles tie every shipped scoring constant to the
form's own key. The maintainer did not adopt it.

The difference matters only for prorated facets (1 to 25% of items missing)
whose prorated sum has a fractional part. Example: a 14-item facet with 3
items missing and a partial sum of 20 gives 20 × 14 / 11 = 25.45. Round-half-up
gives 25 (average 1.786); ceiling gives 26 (average 1.857).

## Materials

Read these, in order:

1. `cairn/references/apa2013pid5irf.md` — the informant key's references page:
   provenance, the item alignment with the self-report form, the two reverse
   lists, and the scoring rule.
2. The informant key's own wording. Run
   `pdftotext -raw cairn/references/sources/apa2013pid5irf.pdf - | grep -n -B30 -A6 "whole number"`
   (the PDF is on the local shelf; sha256 `be4a260a…f8af4d`). Then run the same
   command on `cairn/references/sources/apa2013pid5child.pdf` and
   `cairn/references/sources/apa2013pid5bfchild.pdf` to see the child keys'
   wording. If `pdftotext` is unavailable, say so and rely on the quotations in
   this brief.
3. `cairn/SOURCES.md`, the section "Note on FULL/SF domain scoring" (around
   lines 135–176), which quotes the adult self-report key and records the
   half-up decision. The adult self-report key is not on the shelf; its public
   URL is in the "Sources" list of the same file (the "APA scoring key" entry).
   You may fetch it to read its current wording, and you may search the web for
   the wording of other APA DSM-5 / DSM-5-TR "emerging measures" scoring
   instructions. Cite every page you rely on with its URL and the date read.
4. `R/util.R`, `round_half_up()` and `apa_mean()` (around lines 700–730), and
   `R/score_engine.R` (search for `apa_mean`), to see where the rule sits and
   what a version-specific rule would touch.
5. `R/score_pid5.R` (the `missing` parameter's help text and the function body
   at the end of the file).
6. `cairn/DESIGN.md`: principles IP2 and GP1–GP3 (around lines 95–110) and
   D-009 (around line 184).
7. `cairn/DECISIONS.md`: D-089 (top of the file) and D-088 (the entry after
   it, for how the BFPM version handled a key without a missing-data rule).
8. `cairn/milestones/M159-pid5irf-scoring.md`: the acceptance criteria,
   especially AC2, and the last work-log lines.

## Questions

1. Which proration rounding rule should `score_pid5(version = "IRF")` apply
   under `missing = "apa"`: (a) the informant key's wording read as a ceiling
   (any fractional prorated sum goes to the next whole number), (b) the
   package's current round-half-up rule shared by every PID-5 version, or (c)
   something else? Weigh at least: the plain meaning of "round up to the
   nearest whole number"; the other APA keys' wording, including the adult
   self-report key's current text if you can read it; whether "round up" is a
   house phrasing in APA's emerging-measure keys; the informant key's known copy
   error; whether informant and self-report scores of the same person are
   expected to be compared; and IP2 and GP1.
2. If your answer is (a): state the exact computation, including the treatment
   of a prorated sum that is a whole number and of floating-point results such
   as 10.000000000000002, and say whether the guard belongs in a shared helper
   or a version-specific path. If your answer is (b): say what makes the
   informant key's wording a lapse rather than a rule, and what evidence would
   reopen the choice.
3. AC2 of M159 reads: "Its values equal hand-computed values under the APA
   rules the key prints (reverse, prorate, average)." Under your answer to
   question 1, can AC2 stand as written, or does it need an amendment? If an
   amendment, give the exact replacement wording. Also say what the AC2 fixture
   must contain so that it fails under the rule you did not choose (a prorated
   facet whose fractional part separates the two rules).
4. What should the package say about the choice, and where: the `missing`
   help text and the IRF details section of `?score_pid5`, NEWS, SOURCES.md,
   and the references page? Draft the help-page sentence.
5. Does your answer imply any change for the FULL, SF, BF or BFPM versions?
   Under GP2 any change to their scored values must be explicit and recorded;
   say whether you recommend one, and why or why not.

## Constraints

- D-089 is fixed: the version is `"IRF"`, the items live in `pid_items$IRF` and
  `pid_items$TextIRF`, the reverse-keyed items are the Facet Table's 14, and
  the domains are `pid_domains`. Do not relitigate it.
- D-009 is fixed for the existing versions unless you argue explicitly under
  question 5 for a change.
- IP2: every shipped constant that affects scored output traces to a
  SOURCES.md-cited authority. GP1: published rules are the defaults, and
  deviations are loud. GP2: scored output never changes silently.
- The 25% threshold and the "more than 25% is NA" boundary are the same in
  every key and are not in question.
- If you disagree with a constraint, say so explicitly rather than working
  around it.

## Output format

In `RR07-irf-proration-rounding.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under "Beyond
the brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason. Your report is advisory: emit a `## Binding criteria`
section ONLY if this brief's header slot says `requested`. Where requested:
numbered `BC1…`, each a measurable assertion checkable against evidence,
with any numeric projection stating its tolerance. These are ingested
VERBATIM into the constrained milestone's acceptance criteria and
mechanically diffed against this file; departures are legal only through
that milestone's shown "Deviations from RR07" table.
