# M162: PID-5 forensic form (FFBF) keying and scoring

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, GP1, GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — a new `version` value on exported scoring functions and two new exported tables
- **Branch/PR:** —

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

- [ ] AC1: `pid_ffbf_items` holds the 100 FFBF items in item-number order, 1 to 100. Each item has its facet, its reverse flag and four texts: self and informant report, each in English and German. Each text equals its Table S3 cell under one rule. The rule removes source notes in parentheses, the "(-)" mark and footnote letters on item numbers. It also removes the stray "E14", a leading ellipsis, and a hyphen that the layout adds at a line break. It keeps the printed wording, typos included. `data-raw/check_pid_ffbf_text.R` applies the rule to the shelf PDF, compares all 400 texts, and exits with no difference. A keying test compares each facet's 4 item numbers and the reverse list with a transcription. The transcription is typed into the test file, not derived from the package tables.
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

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3, T4
- AC4 → T4
- AC5 → T3, T4
- AC6 → T5

## Tasks

- [ ] T1: Write the references page `cairn/references/niemeyer2022.md` (paper, Table S3, code, and the local codebook as a pre-final draft) and its INDEX line. Add the SOURCES.md row. Transcribe Table S3 into `data-raw/pid_ffbf_items.csv`. Write `data-raw/check_pid_ffbf_text.R` (AC1) and record its run on the references page. Compare the facet lists, reverse items and domain facets with the code. (RB tripwire: ip-touching)
- [ ] T2: Build `pid_ffbf_items`, `pid_ffbf_domains` and `pid_scales$FFBF` in `data-raw/pid_info.R`, and document them in `R/data.R`. Write the keying tests (AC1, AC2). Keying content needs Jeff's sign-off before merge.
- [ ] T3: Thread `"FFBF"` through `score_pid5()` (reverse flags from `pid_ffbf_items`, domains from `pid_ffbf_domains`), `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()`.
- [ ] T4: Write the fixture and recomputation tests (AC3, AC5) and the `"F"` test. Add the IRF pairing to the characterization script and run it at the merge base and at the head (AC4).
- [ ] T5: Update the help pages, vignette, NEWS and `_pkgdown.yml`. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

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

## Decisions

## Review
