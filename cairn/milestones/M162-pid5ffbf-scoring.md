# M162: PID-5 forensic form (FFBF) keying and scoring

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, GP1, GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — a new `version` value on exported scoring functions and two new exported tables
- **Branch/PR:** m162-pid5ffbf-scoring

## Goal

Researchers can score the 100-item PID-5 Forensic Faceted Brief Form (PID-5-FFBF, Niemeyer et al., 2022), self and informant report, with `score_pid5()` and `reliability_pid5()`.

## Scope

**In:**
- Keying and item text enter through `data-raw/`. The source is Table S3 of the paper's supplement (`cairn/references/sources/niemeyer2022_tableS3.pdf`, from osf.io/9eyrv). The authors' analysis code (`niemeyer2022_code.R`, lines 109 to 190) is a second source for the facet lists, the reverse items and the domain facets. The paper (`niemeyer2022.pdf`, p. 33) gives the APA three-facet domain rule and the 0 to 3 scale.
- A new exported table `pid_ffbf_items` holds the 100 items in item-number order. Each item has its facet, its reverse flag, and its self and informant text in English and German. A new exported table `pid_ffbf_domains` holds 7 domains. The 5 APA domains take the `pid_domains` facets. The paper's two new domains are Disinhibited Aggression (Emotional Lability, Hostility, Impulsivity) and Insecurity (Separation Insecurity, Anxiousness, Perceptual Dysregulation). `pid_scales` gains `FFBF`.
- `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` accept `version = "FFBF"`. Item columns are `pid5ffbf_001` to `pid5ffbf_100`. One version scores the self and the informant form, as D-091 did for the child forms. `label_pid5()` labels with the English self-report text.
- A references page, a SOURCES.md row, tests, NEWS, help pages and a vignette section.

**Out:**
- The English Word, Qualtrics and REDCap forms go to M163.
- German forms go to the "Instruments awaiting materials" candidate row, because no source on the shelf prints all four German response labels.
- The FFBF validity scales go to the same row. Table S3 marks some items as SD-TD, PRD and INC-S items, but no FFBF validity key is published. `validity_pid5()`, `norm_pid5()` and `plot_pid5()` keep their versions, because no FFBF norms are published (IP3).
- Averaging two informants item by item is the user's step, as in the paper. The help page states it, and no function does it.
- Module Builder, hitop-form and Study Link Builder support go to the same row.

## Acceptance criteria

- [ ] AC1: `pid_ffbf_items` holds the 100 FFBF items in item-number order, 1 to 100. Each item has its facet, its reverse flag and four texts: self and informant report, each in English and German. Each item's four texts, joined in the order German self, English self, German informant, English informant, equal its four Table S3 cells joined in reading order under one rule. The rule joins a cell's lines with one space and collapses runs of spaces. It removes a parenthetical source note that begins "(G" or "(E-", the "(-)" mark, the stray markers E14, E18 and E77, a leading ellipsis and a final period. It turns typographic quotes into straight quotes, and typographic apostrophes and the acute accent into a straight apostrophe. In a German text it removes a hyphen that a lowercase letter follows, at a line break or not. In an English text it joins a word that a line break splits after a hyphen, keeping the hyphen. It keeps the printed wording otherwise, typos included. `data-raw/check_pid_ffbf_text.R` reads `pid_ffbf_items` from `data/` and applies the rule to the shelf PDF. It compares the item order, the facets and all 400 texts, and exits with no difference. A keying test compares each facet's 4 item numbers and the reverse list with a transcription of Table S3's facet headings and "(-)" marks. The transcription is typed into the test file, not derived from the package tables or the CSV.
- [ ] AC2: `pid_ffbf_domains` holds 7 rows. The 5 APA domains have the primary facets of `pid_domains`. Disinhibited Aggression and Insecurity have the facets of the authors' code (lines 184 and 185). The keying test compares all 7 facet sets with a transcription typed into the test file.
- [ ] AC3: `score_pid5(version = "FFBF")` returns 25 facet columns in `pid_scales$FFBF` row order. Then come the 5 APA domain columns, `disinhibitedAggression` and `insecurity`. Each name starts with `prefix`. The facet and APA domain columns are named as the SF version names them. Its values equal values computed by hand under the SF version's rules. A reverse item scores as 3 minus the response. A facet with 25% or less missing is prorated and rounded as D-009 states. A domain is the mean of its 3 facets. The test fixture has at least 5 respondents, and it holds each of these cases:
  - Each reverse item takes 0 for one respondent and 3 for another.
  - One respondent's missing item is a reverse item.
  - A facet with 1 of 4 items missing has a partial sum that is not a multiple of 3.
  - A facet with 2 of 4 items missing goes `NA`.
  - Emotional Lability goes `NA`, so Negative Affectivity and Disinhibited Aggression both go `NA`.
  - One call uses a non-default `srange`.

  The expected values are typed into the test. A second test recomputes all 32 scores on random answers with `NA`s under each of the three `missing` modes. It uses key tables typed into the test.
- [ ] AC4: The output of the existing versions does not change. `data-raw/characterize_bfpm.R` gains an IRF pairing built as its BFPM pairing is built. It runs at the merge base and at the branch head, and every call it makes gives `identical()` output at both. A test shows that `version = "F"` errors in the four functions that gain FFBF and still gives FULL in `validity_pid5()`, `norm_pid5()` and `plot_pid5()`.
- [ ] AC5: `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` each have a test on the FFBF version. `reliability_pid5()` returns the 25 facet rows, as it does for the SF. `label_pid5()` labels each of the 100 items with its English self-report text and each of the 32 score columns with its scale name.
- [ ] AC6: The help pages and `vignettes/pid5_scoring.Rmd` name the FFBF and cite Niemeyer et al. (2022). They state six facts:
  - The form was validated in German.
  - The English text is the authors' translation.
  - Informant data use the same version, and the user averages two informants item by item before scoring, as the paper does.
  - The paper's Antagonism and Detachment are the APA domains.
  - The missing-data rule is the SF rule, not the authors' code rule, which does not round and averages a domain over its 12 items.
  - `label_pid5()` uses the self-report text.

  NEWS.md has an entry that also says `version = "F"` no longer abbreviates `"FULL"` in the four functions. The `_pkgdown.yml` reference index lists both new tables. `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T1, T2, T6
- AC2 → T1, T2
- AC3 → T3, T4
- AC4 → T4
- AC5 → T3, T4
- AC6 → T5

## Tasks

- [x] T1: Write the references page `cairn/references/niemeyer2022.md` (paper, Table S3, code, and the local codebook as a pre-final draft) and its INDEX line. Add the SOURCES.md row. Transcribe Table S3 into `data-raw/pid_ffbf_items.csv`. Write `data-raw/check_pid_ffbf_text.R` (AC1) and record its run on the references page. Compare the facet lists, reverse items and domain facets with the code. (RB tripwire: ip-touching)
- [x] T2: Build `pid_ffbf_items`, `pid_ffbf_domains` and `pid_scales$FFBF` in `data-raw/pid_info.R`, and document them in `R/data.R`. Write the keying tests (AC1, AC2). Keying content needs Jeff's sign-off before merge.
- [x] T3: Thread `"FFBF"` through `score_pid5()` (reverse flags from `pid_ffbf_items`, domains from `pid_ffbf_domains`), `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()`.
- [x] T4: Write the fixture and recomputation tests (AC3, AC5) and the `"F"` test. Add the IRF pairing to the characterization script and run it at the merge base and at the head (AC4).
- [x] T5: Update the help pages, vignette, NEWS and `_pkgdown.yml`. From RR08, the FFBF help also states: the five missing-data facts of RR08 section 4; the informant facts of section 5 (item-by-item averaging, the other informant's value when one is missing, half-integer values and the `"apa"` rounding); that FFBF item numbers are not `pid_items$SF` numbers; and the forensic domains' caveats (exploratory single sample, Insecurity's open status, shared facets). Open SOURCES.md OQ-6 (item 10's wording in the article against Table S3) and OQ-7 (the English typos the CSV keeps). Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.
- [ ] T6: From RR08: extend `data-raw/check_pid_ffbf_text.R` to the code's informant lists (lines 135 to 159), its original-form lists (3311 to 3361), its informant recodes, and the count of unadapted items against Table S3's a, b and c marks. Rerun it and record the run on the references page.

## Work log

- 2026-10-04: created by /milestone-plan, together with M163. The ROADMAP row said "OSF holds only a pre-final item pool". This was wrong: the supplement component (osf.io/fzyvr) holds Table S3 and the code component holds the authors' code. With Jeff's permission, both were downloaded to the shelf.
- 2026-10-04: question set: download Table S3 and the authors' R code from OSF? Answer: both.
- 2026-10-04: question set: which item text ships? Answer: English and German in the table. The other answers: a new `pid_ffbf_items` table, the 5 APA domains plus Disinhibited Aggression and Insecurity, and one version for self and informant data.
- 2026-10-04: question set: German forms? Answer: English forms only, German forms a candidate. Text permission? Answer: Jeff gives it before M163 merges.
- 2026-10-04: plan gate chose a separate `pid_ffbf_items` table over 100 rows appended to `pid_items`. D-089 rejected a separate table for the IRF. But 82 FFBF items are rewritten and match no 220-item row. Appended rows will also break the rule of one row per PID-5 item. Falsified by a later form needing a third lookup path into the scoring engine.
- 2026-10-04: plan chose the SF version's missing-data rule over the authors' code rule. The code prorates a facet with 1 missing of 4 unrounded and averages a domain over its 12 items with up to 3 missing. The paper prints no missing rule, and the package applies the APA rule to the SF by analogy (D-009). The help page states the difference. Falsified by an FFBF publication that prints a missing-data rule.
- 2026-10-04: plan chose a 7-row `pid_ffbf_domains` table over domain rows in `pid_scales$FFBF`, following D-088(c). Falsified by a domain rule that is not a mean of 3 facets.
- 2026-10-04: criteria audit (full mode, fresh Opus reader) found that Table S3 and the code agree on all 25 facets, the reverse items 12 and 26 and both forensic domains. It returned 9 findings on M162, all applied. AC1 now uses item-number order and a text rule, and it binds the texts, not the record. AC3 fixes the column order and replaces probes that could not fail. AC4 names the IRF pairing and adds the `"F"` test. AC6 adds the missing-data and informant facts.
- 2026-10-04: audit judgment on AC5, decided toward the narrower promise: `reliability_pid5()` returns the 25 facet rows, as for the SF, not 32 rows with domains as for the BF+M.
- 2026-10-04: implement started on branch m162-pid5ffbf-scoring. Untracked devel/hitopdat_* files predate this milestone and stay unstaged.
- 2026-10-04: T1 done. `data-raw/pid_ffbf_items.csv` built from `pdftotext -bbox` word positions (builder kept in the session scratchpad, method on the references page). `data-raw/check_pid_ffbf_text.R` reads the PDF with `pdftotext -raw` and the code file: PASS. Seven planted defects (moved, changed and dropped words, wrong facet, extra reverse flag, a changed shipped text, a reordered shipped table) each exit 1. References page `niemeyer2022.md`, INDEX line and three SOURCES.md rows added.
- 2026-10-04: T1 found three source facts the AC1 rule did not name: stray markers E14, E18 and E77 (not E14 alone), syllable hyphens inside German words ("Ge-fühle", "Be-ziehungen"), and an acute accent in "doesn´t". The CSV follows a rule that covers them. AC1's wording awaits Jeff (stop below).
- 2026-10-04: re-audit: AC1 (full) — 4 findings, all taken toward the narrower promise: the script reads the shipped `pid_ffbf_items`, the test transcription is stated as Table S3's, the three markers are named, and the claim is "four cells in reading order".
- 2026-10-04: re-audit: AC1 (full) — 3 findings on the fixed wording: the reading-order claim can still pass a word moved across a cell boundary where all four cells share one raw line (item 36); name the ASCII targets and the hyphen cases exactly; state the line-join and space rule. The second line on AC1 is the stop, so no further reader runs.
- 2026-10-04: T2 done. `pid_ffbf_items` (100 x 7), `pid_ffbf_domains` (7 rows) and `pid_scales$FFBF` built by `data-raw/pid_info.R`; the five older `pid_scales` elements are `identical()` to HEAD and the other `data/` files are byte-identical. Docs in `R/data.R`. Typed key tables in `helper-fixtures.R`, keying tests in `test-keying.R`, `test-column-shape.R` admits the new element. Full suite green apart from the known skips.
- 2026-10-04: implement chose the FULL facet order for `pid_scales$FFBF` over Table S3's alphabetical order, so FFBF output columns line up with the other 25-facet versions. Falsified by a user need to read output in form order.
- 2026-10-04: implement named the two forensic domains as the paper prints them ("Disinhibited Aggression", "Insecurity") under D-018, while the APA rows keep `pid_domains` spelling.
- 2026-10-04: substantive amendment: AC1's text rule now names the three stray markers, the German in-word hyphen, the acute accent and the line-join rule, and AC1 promises each item's four texts in reading order, as the check proves. Jeff chose this wording at the stop. The data did not change.
- 2026-10-04: stop (RB tripwire: ip-touching, T1): Jeff chose to escalate the FFBF keying and domain sources via /milestone-brief rather than accept it at this point.
- 2026-10-04: blocked on RB08 (FFBF keying, forensic domains and missing-data rule). The brief is committed on the milestone branch, as RB07 was for M159.
- 2026-10-04: ingested RR08 (Fable, advisory). Keying, reverse items, domains and the SF missing rule stand as built; D-092 records them. T5 gains the RR08 help-page facts and OQ-6 and OQ-7; T6 is new (check-script extension, minor amendment adding a discovered sub-task). Status back to in-progress.
- 2026-10-04: T3 done. `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` take `"FFBF"`: reverse items from `pid_ffbf_items`, domains from `pid_ffbf_domains`, stem `pid5ffbf_`. `version = "F"` now errors in these four (ambiguous with FFBF). Suite green.
- 2026-10-04: implement chose to let `rename_pid5_items(version = "FFBF", method = "text")` match any of the form's four texts (English or German, self or informant) over the self-report English text alone, because the form was given in German and informant exports carry informant wording. Falsified by two FFBF texts that are equal across versions yet name different items.
- 2026-10-04: T4 done. `fx_pid5ffbf()` (6 respondents) and `test-score_pid5ffbf.R`: hand-computed values for all 32 columns, the 1 to 4 range, a recomputation from the typed tables under all three `missing` modes, the `"F"` test, and reliability, rename and label tests. Two planted scoring defects (reverse list cut to item 12; APA domain map in place of the FFBF map) gave 10 and 14 failures. `characterize_bfpm.R` gained the IRF pairing: 120 of 120 calls `identical()` at merge base 54507a94 and at the head. Suite green.
- 2026-10-04: implement revised the facet-order choice: `pid_scales$FFBF` follows the SF's facet order, not the FULL order, because the SF and FULL orders differ (found by the T4 test) and the FFBF adapts the SF, so FFBF and SF columns now line up. Falsified by a user need to read output in Table S3 order.
- 2026-10-04: T5 done. FFBF help section in `?score_pid5` (keying, output, domain caveats, the missing-data comparison, informant averaging and half-integer rounding, both checked by a run), version docs in the three other functions, the vignette section (purled and run), NEWS entry, `_pkgdown.yml` rows. OQ-6 and OQ-7 were opened at the RR08 ingest. `pkgdown::check_pkgdown()`: no problems; `devtools::check()`: 0 errors, 0 warnings, 0 notes.

## Decisions

- 2026-10-04 (RR08 Q1): Table S3, the code's self, informant and original-form lists all give facet k = items k, k + 25, k + 50, k + 75. Table S3 governs item content and facets if sources ever differ. Applied: no change.
- 2026-10-04 (RR08 Q2): items 12 and 26 are the only reverse items of both forms (Table S3, code lines 382 to 388, article p. 32). No unmarked reverse wording among the 200 English texts. Applied: no change.
- 2026-10-04 (RR08 Q3): the three-facet forensic domains are the paper's published definition (pp. 35, 36, 38, 39), not only a script choice. Promoted to D-092(c). Help caveats scheduled in T5.
- 2026-10-04 (RR08 Q4): keep the SF missing-data rule; the code's domain rule contradicts the paper's three-scale definition under missing data, and D-090 rejected version-specific rules. Promoted to D-092(d). Five help-page facts scheduled in T5. An FFBF-specific `missing` mode: rejected (RR08's reason).
- 2026-10-04 (RR08 Q5): no informant keying difference. Three averaging facts scheduled in T5.
- 2026-10-04 (RR08 Q6): no transcription change alters item wording under IP1. Applied: the references page now says why hyphens and the accent are corrected while misspellings are kept.
- 2026-10-04 (RR08 beyond): item 10's article wording and five English typos become SOURCES.md OQ-6 and OQ-7 (T5, written at this ingestion); the check-script extension is T6; Table S3's d, e and f letters are recorded on the references page now. Re-transcribing the source notes: rejected, because they stay in the shelf PDF and the check script already parses them. Separate four-factor Antagonism and Detachment columns: rejected (RR08's reason). The informant stem and labels for M163 go to M163's work log.

## Review
