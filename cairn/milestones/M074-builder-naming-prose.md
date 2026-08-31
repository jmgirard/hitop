<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M074: The builder page's naming prose states the conditions it actually depends on

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m074-naming-prose` (this repo), PR https://github.com/jmgirard/hitop/pull/80; builder PR https://github.com/jmgirard/hitop-builder/pull/11

## Goal

The browser module builder's copy states the condition each of its download-naming
claims actually rests on, and tells a visitor holding pre-rename downloads which
names replaced theirs.

## Scope

Surface tier: **user-facing** — every line this milestone changes is read by a
visitor or a researcher on the page itself. D-038 puts the page's own copy
outside IP1: *"The page's own copy — notices, labels, the README — is
researcher-facing and outside IP1's scope, which names participant-facing
text."* No sign-off gate stands in front of it.

The work lands in the sibling repo `jmgirard/hitop-builder` (single-file
`index.html` plus `README.md`), on a branch and PR there; this repo carries only
the tracking record, as it did for M051, M052, M053 and M063.

**In:** the download hint's account of why the scoring-file button switches off
during a build (`index.html:548-555`) and the same sentence in `README.md`; the
README's account of when a name carries `-module` (`README.md:164` and the
"Ticking all 76 scales" passage of the same shape); a dated line naming the two
stems the page wrote before M063 and the eight that replaced them; the paragraph
under the README's naming table and the code comment above `downloadStem`, both
of which say the scoring file records the paper size, the item numbering and the
online naming values.

**Out:**
- A light/dark control of the page's own → stays on the builder-page candidate
  row, not planned at this gate.
- The client-facing definition text and the 17 subscale definitions → their own
  candidate row, split from the builder-page row at this gate.
- Any change to what the page builds, and any generator argument the page does
  not already pass → the maintainer sign-off gate D-036 and D-037 each closed
  with.
- Any change to the `hitop` R package.

## Acceptance criteria

- [x] AC1: The page's and README's account of why the scoring-file button
      switches off while a build runs names the condition under which the held
      scoring file's own name equals the name the running build's descriptor
      takes — the same format, the same scope, and the same shuffle setting —
      rather than asserting the collision for every rebuild. Verified against a
      computed sweep over all 64 ordered pairs of the eight builds M063-D1
      tabulates, marking each pair's two stems equal or not.
- [x] AC2: The README's account of when a download's name carries `-module`
      states the `tilesExactly` gate `wholeInstrument()` applies
      (`index.html:745-747`), and predicts the name saved by each of four builds
      driven on the served branch page: `tilesExactly` in {true, false} crossed
      with {every scale ticked, a named two-scale selection}.
- [x] AC3: The page carries a dated line telling a visitor holding downloads
      made before the rename both which names the page wrote before it and which
      names carry their place now, written so it can be deleted in a later
      milestone without disturbing the surrounding copy. The old stems it names
      are set-equal to the complete set the stem expression at builder commit
      `9f8b615` could write; the new stems it names are set-equal to the eight
      `downloadStem` produces.
- [x] AC4: No file the page builds changes across this milestone, over the
      matrix {a named two-scale selection, every scale} × {Word: US Letter and
      A4, numbering 1-to-n and original, shuffle off} × {Qualtrics and REDCap:
      the package defaults and one named non-default naming set} — 16 builds,
      each driven on the served branch page and on the deployed page in one
      session with both pages reporting the same `hitop` version, and compared
      on the questionnaire (Qualtrics `.txt` byte for byte, REDCap
      `instrument.csv` line for line, Word on parsed header text and printed
      item rows, a DOCX not being byte-reproducible) and on the `.json`
      descriptor byte for byte.
- [x] AC5: The `hitop` package is untouched: `git diff --name-only
      origin/main...HEAD` in this repo lists only files under `cairn/`, and
      `Rscript -e 'devtools::test()'` reports 0 failures.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T5
- AC4 → T6
- AC5 → T7

## Tasks

- [x] T1: Compute the collision map — the eight stems `downloadStem(format,
      whole, shuffle)` produces for M063-D1's eight builds, then all 64 ordered
      pairs marked equal or not. Prove the comparison able to fail by running
      two deliberately wrong predicates against it, one asserting a collision on
      every pair and one on none; both must land in the disagreement list.
- [x] T2: Rewrite the download hint (`index.html:548-555`) and its README twin
      so the reason names the same-format, same-scope, same-shuffle condition.
      Transcribe the shipped sentence into a predicate by reading that sentence
      alone, without the map open, then compare it against T1's 64 pairs.
- [x] T3: Rewrite the README's `-module` sentence (`README.md:164`) and the
      "Ticking all 76 scales" passage of the same shape to state the
      `tilesExactly` gate.
- [x] T4: Drive the four AC2 builds on the served branch page, recording each
      saved name off a patched `HTMLAnchorElement.click` rather than a reported
      coordinate (the M056 lesson). Force `tilesExactly` false by overriding the
      value its R probe sets (`index.html:1400-1407`) before the picker is
      built.
- [x] T5: Add the dated rename line to the page, naming `hitopsr` and
      `hitopsr-module` and what replaced them. Read the old set off builder
      commit `9f8b615`'s stem expression and the new set off `downloadStem`, and
      compare both against what the line says, read back from the served page.
- [x] T6: Run the AC4 16-cell regression across the branch page and the deployed
      page in one session, comparing questionnaires and descriptors.
- [x] T7: Open the builder PR from `m074-naming-prose`; confirm this repo's
      branch diff is tracking-only and `devtools::test()` clean. After the
      builder merge, confirm the deployed page is byte-identical to it.

## Work log

- 2026-08-30: created by /milestone-plan, promoting the M063 half of the builder-page candidate row.
- 2026-08-30: criteria audit ran in FULL mode (user-facing tier) in a fresh reader that authored none of the criteria; returned findings on 4 of this milestone's 5 drafted criteria — three checker-property clauses in the promises, a plant aimed at the mechanical half of AC1's check rather than the transcription, AC2 driving only all-ticked builds, AC3 checking the old stems but not their successors, and AC4 quantifying over paper size, numbering, naming values and every selection while enumerating eight filenames and omitting the descriptor. All fixed before the criteria were written.
- 2026-08-30: the audit also argued the handover sentence describes a mechanism that cannot exist, `offerDescriptor` storing the held file under its own name (`index.html:1089`); checked and not adopted — that held name equals the running build's descriptor name exactly when the two stems coincide, which is the condition AC1 asks the sentence to state.
- 2026-08-30: plan gate chose one milestone for the three M063 prose items over one covering all six items on the builder-page row, because the row's own promotion condition has fired only for the prose ("when the builder's prose is next edited"); falsified by a visitor asking for a theme control or a clinician asking for the client wording in the picker, either of which promotes its half.
- 2026-08-30: plan gate chose a dated, removable rename line over a permanent one and over no line at all, because the rename is a one-time event whose notice should not outlive the visitors holding old files; falsified by the line still earning its place when a later milestone next edits that copy.
- 2026-08-30: implementation started on `m074-naming-prose`; builder branch of the same name cut from jmgirard/hitop-builder `main` at `2a7f2ae`.
- 2026-08-30: T1 — collision map built by extracting `INSTRUMENT`, `FORMATS` and `downloadStem` out of the branch `index.html` and evaluating them over M063-D1's eight builds: 8 of the 64 ordered pairs collide, each a build with itself. The two wrong controls both disagreed with it (all-collide 56, none-collide 8).
- 2026-08-30: T2 — page hint and README twin rewritten to name the same-format, same-scope, same-shuffle condition (builder `a283dcf`). A fresh [S] reader given only the two shipped sentences, with no map and no repository access, transcribed them to `held.format === running.format && held.whole === running.whole && held.shuffle === running.shuffle`, which disagrees with the map on 0 of 64 pairs.
- 2026-08-30: T3 — both README passages rewritten to state the start-up gate `wholeInstrument()` applies, naming the page's own log line as where a reader sees the answer (builder `e6b48ee`). The probe checks the union of every scale's items equals 1..N, so the prose says "nothing left out" rather than the no-overlap claim the surrounding code comment makes.
- 2026-08-30: T4 — four Word builds driven on the served branch page (localhost:8788, hitop 0.2.0), each saved name read off a patched `HTMLAnchorElement.prototype.click` and the descriptor name off the handover button. Gate true / all 76 ticked gave `hitopsr-word.docx` + `hitopsr-word.json`; gate true / {agoraphobia, insomnia} gave `hitopsr-word-module.*`; gate forced false (served copy assigning `tilesExactly = false` after the probe) gave `hitopsr-word-module.*` for all 76 ticked and for the same two scales. The false / all-76 cell is where the old wording and the new one disagree, and the page took `-module`.
- 2026-08-30: T5 — dated rename paragraph added inside the download notice, its own `<p id="renameNote">` behind a comment saying it is deletable in one block (builder `a5267a6`). The ten names were read back out of the served page's rendered `#renameNote`, then split against the eight `downloadStem` produces: the remainder is set-equal to the two the `9f8b615` expression could write, and the rest set-equal to the eight, with no name unaccounted for. Two doctored read-backs (one name dropped, one invented name added) each failed the comparison.
- 2026-08-30: T6 — the 16-cell matrix built twice in one session, on the served branch page and on https://jmgirard.github.io/hitop-builder/, both logging hitop 0.2.0 and the deployed one carrying no rename note. Each cell's questionnaire and descriptor were captured off a patched `URL.createObjectURL` plus anchor click and reduced to a fingerprint over the Qualtrics `.txt` byte digest, the REDCap `instrument.csv` line digest and member list, the Word header text and anchored `<w:tr[ >]` row digest, and the descriptor byte digest and length; raw DOCX size is excluded, the zip stamps not being reproducible. All 16 fingerprints matched; a doctored copy of one cell mismatched, and the loose `<w:tr` count (3x the anchored one) is recorded per Word cell.
- 2026-08-31: scope amended at a mini gate to add a fourth prose site — the paragraph under the README's naming table and the code comment above `downloadStem` both said the scoring file records the paper size, the item numbering and the Qualtrics/REDCap naming values. It records none of them: the eight two-scale cells of T6's regression span both paper sizes, both numbering modes and default/custom naming, and every descriptor hashed to `d877679172cacc1464445258f9c7c8d634cef559a8004683dfe6ab7b122786a8`. Jeff chose fixing it here over a candidate row; both sites corrected in builder `0a8780c`, no acceptance criterion changed.
- 2026-08-31: the 16-cell matrix re-run on the branch page after the last two prose commits. Every questionnaire digest, name, row count and loose/anchored `<w:tr` count matched the earlier run; the descriptors differ only in `buildDate`, and substituting `2026-08-30` back into one reproduced the earlier digest exactly.
- 2026-08-31: T7 — builder PR https://github.com/jmgirard/hitop-builder/pull/11 opened from `m074-naming-prose`. This repo's branch diff against `origin/main` lists only `cairn/ROADMAP.md` and the milestone file; `devtools::test()` reports FAIL 0, WARN 0, SKIP 4, PASS 16208. The post-merge check that the deployed page matches the merged file is left to review, which is where the merge happens.
- 2026-08-31: all tasks done, status set to review.
- 2026-08-31: review ran on `m074-naming-prose`; all five criteria verified with fresh evidence, consistency gate clean (`cairn_validate` exit 0, `document()` no diff, `check_pkgdown()` clean, `R CMD check` 0/0/0), three-lens fan-out returned six findings from the [O] lens and none from the other two.

## Decisions

## Review

Reviewed 2026-08-31 on `m074-naming-prose`. This repo: PR
https://github.com/jmgirard/hitop/pull/80 (tracking only). Builder: PR
https://github.com/jmgirard/hitop-builder/pull/11 at `0a8780c`, whose
`origin/main` base `2a7f2ae` is byte-identical to the page deployed at
https://jmgirard.github.io/hitop-builder/ (`f191d835…` both).

**AC1 — evidence.** `INSTRUMENT`, `FORMATS` and `downloadStem` extracted from
the branch `index.html` and evaluated over M063-D1's eight builds: eight
distinct stems, so of the 64 ordered pairs exactly 8 collide, each a build with
itself. The condition the shipped page hint and its README twin state — same
format, same scope, same shuffle — transcribed as a predicate disagrees with
that map on 0 of 64 pairs; the two wrong controls disagree on 56 (all collide)
and 8 (none collide). Neither sentence asserts a collision for every rebuild.

**AC2 — evidence.** Four Word builds driven on the served branch page
(localhost:8788, hitop 0.2.0), each saved name read off a patched
`HTMLAnchorElement.prototype.click` and the descriptor name off the handover
button. Gate true (the page's own probe) with all 76 ticked gave
`hitopsr-word.docx` / `hitopsr-word.json`; gate true with {agoraphobia,
insomnia} gave `hitopsr-word-module.*`; gate forced false — a served copy
assigning `tilesExactly = false` after the probe — gave `hitopsr-word-module.*`
for all 76 ticked and for the same two scales. The README's rewritten passages
predict all four, including the discriminating false/all-76 cell. The criterion
cites `index.html:745-747` for the gate; the added prose shifted it to
`wholeInstrument()` at `index.html:764-766`, same function, same gate.

**AC3 — evidence.** Ten `<code>` names read back out of the served page's
rendered `#renameNote`: the eight that are set-equal to what `downloadStem`
produces, and a remainder set-equal to the two the stem expression at builder
`9f8b615` could write, with no name unaccounted for. Two doctored read-backs
(one name dropped, one invented name added) each failed the comparison.
`renameNote` occurs exactly once across `index.html` and `README.md` — no CSS
rule, no script reference — so the paragraph and its comment delete as one
block.

**AC4 — evidence.** The 16-cell matrix built twice in one session, on the
served branch page and on the deployed page, both logging hitop 0.2.0. Each
cell's questionnaire and descriptor were captured off a patched
`URL.createObjectURL` plus anchor click and reduced to one fingerprint over the
Qualtrics `.txt` byte digest, the REDCap `instrument.csv` line count and
digest, the Word header text and anchored `<w:tr[ >]` row digest and count, and
the descriptor byte digest and length; raw DOCX size is excluded, the zip
stamps not being reproducible. All 16 fingerprints match. The fingerprint
discriminates: it separates the two paper sizes, the two numbering modes on the
two-scale Word cells, and default from custom naming on both online formats,
and two planted defects (one flipped `instrument.csv` digest byte, one altered
Word header word) each changed it.

**AC5 — evidence.** `git diff --name-only origin/main...HEAD` lists
`cairn/ROADMAP.md` and `cairn/milestones/M074-builder-naming-prose.md` and
nothing else. `Rscript -e 'devtools::test()'` reports FAIL 0, WARN 0, SKIP 4,
PASS 16208.

**Consistency gate.** `cairn_validate.py` exit 0, all checks pass; the 22
advisories are the standing pre-existing ones the fourteenth hygiene stamp
records (21 dangling `D-0NN` tokens into `legacy/LOG.md`, one references
staleness WARN), and `release window` did not fire. Profile `r-package`
toolchain slot: `devtools::document()` left `man/` and `NAMESPACE` unchanged;
`pkgdown::check_pkgdown()` reports no problems; `README.Rmd`/`README.md`
untouched on this branch; no `NEWS.md` entry is owed, the package being
untouched; no new top-level files, so no `.Rbuildignore` entry; `Rscript -e 'devtools::check()'`
reports 0 errors, 0 warnings, 0 notes (14m 43s).

**Independent review — three-lens fan-out** (declared tier is user-facing, so
the full fan-out ran even though this repo's own diff is tracking-only; all
three reviewers read the builder diff).

*[S] blame-history* — no history conflict. It read the builder log and blame on
the rewritten regions, M063's archive, and the D-034..D-039 entries, and
reported that each change either narrows an M063 claim to a truer condition or
closes a claim M063's own review had already flagged false and deferred. Zero
defects.

*[S] prior-review record* — no prior-review evidence to contradict. The GitHub
probe returned `[]` inline review comments on both repos, so that surface was
not walked; the archived `## Review` sections for M051, M052, M053, M056, M062,
M063, M064, M065 were read instead. It checked hardest that the diff does not
resurrect the unscoped "two builds never share a name" claim M063's review
removed, and found it does not. Zero findings.

*[O] diff-bug* — six findings, ranked. Each was verified against the
implementation before triage.

1. `index.html` `#renameNote`: "Those two names are now eight, one for each
   build this page can make" is false and contradicts the same notice's own
   paragraph two above it ("Two builds differing only in which scales you
   ticked do share a name"). Confirmed by reading the notice: `downloadStem`
   reads only format, scope and shuffle, so many distinct builds land on one of
   the eight names. A visitor could conclude no build can overwrite another's
   files — the hazard the rest of the notice exists to warn about.
2. `index.html` `#downloadHint` and its README twin state the scope half of the
   collision condition as "every scale or a selection", where the code's gate is
   `wholeInstrument()` — `tilesExactly && selected().length === scales.length`.
   The two coincide on the HiTOP-SR, the only instrument this page builds, so
   the claim is latently rather than actually false; but the milestone's own new
   README passage insists on exactly that distinction, so the diff draws it in
   one place and erases it in another.
3. `README.md` under the naming table: "the questionnaire itself is the only
   record of them" is over-general. Confirmed at `index.html:1227-1230` — the
   page's own build log prints `renumber = FALSE` on original numbering and the
   Qualtrics/REDCap naming values verbatim. Only the paper size is
   questionnaire-only.
4. `README.md` under the naming table and the `downloadStem` comment both
   enumerate "the paper size, the item numbering and the Qualtrics and REDCap
   naming values" as what reaches neither name nor descriptor, omitting REDCap's
   "Mark every item as required", which is a fourth setting in that position.
   Both passages are newly written by this milestone.
5. Two code comments (`index.html` above `tilesExactly`, and above the R probe)
   claim the probe detects scale *overlap*. It cannot: `hitop_module()` computes
   `items <- sort(unique(unlist(...)))` (`R/module.R:102`), so duplicates are
   dropped before `identical(as.integer(items), seq_along(items))` runs.
   Confirmed. Both comment lines are unmodified by this diff.
6. This milestone file's Scope cites the download hint at `index.html:548-555`
   and AC2 cites `wholeInstrument()` at `index.html:745-747`; both are one line
   short of the pre-change element, and the gate now sits at 764-766.

**Triage.**
Findings 1-4 fixed now, at Jeff's disposition at the gate, in builder
`9dcef4b`: the rename note names "one for each format, scope and shuffle
setting the page builds"; the page hint and its README twin say "the whole
instrument or a selection" where they said "every scale or a selection"; the
README's naming-table paragraph now says the questionnaire is the only *file*
that keeps the paper size, numbering and naming values, "though the build log
names all but the paper size while the page stays open"; and REDCap's required
flag joins that enumeration in both the README paragraph and the `downloadStem`
comment. AC1's 64-pair sweep and AC3's two set-equalities were re-run against
the changed wording and both still hold; the served page renders the ten names
and the rewritten hint unchanged otherwise, and no code path was touched, so
AC4's 16 cells stand. No added line exceeds the file's wrap width.

Finding 5 → follow-up candidate row (a pre-existing claim on lines this diff
never modified). Finding 6 rejected: the citations sit in plan-owned text,
which review does not edit, and the Review section above records the gate's
current location.

No finding demonstrated an acceptance criterion failing, and none met the
load-bearing-defect bar for a return; the return floor did not fire.
