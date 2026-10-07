# M171: APA notice in PID-5 online exports and adult Word footers

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — changes the distributed forms of five APA instruments
- **Branch/PR:** —

## Goal

Every distributed Word, Qualtrics and REDCap form of the five APA PID-5 forms carries the copyright and permission notice that its APA PDF prints.

## Scope

**In:**
- The five APA forms are the adult full and brief forms, the informant form (IRF), and the child full and brief forms.
- The adult full and brief notices are stored with the other instruction text and checked against the shelved DSM-5-TR adult PDFs. Jeff puts those PDFs on the shelf before T1 (question set, 2026-10-07).
- The adult full and brief Word footers print their APA notice in place of the Society line, as the informant and child footers already do.
- The Qualtrics and REDCap exports of the five forms print the notice once, after the last item.
- The rebuilt files ship with new manifest rows.

**Out:**
- The Word layout (identity lines, instruction label, continuation text, "Clinician Use" column) goes to part (d) of the candidate row "Form text awaiting a source, sign-off or permission", which waits on APA's written permission. Jeff chose to ask APA first.
- The notice in the PID-5 JSON exports (`pid5.json`, `pid5bf.json`) needs a new format string (D-063). It goes to part (e) of the same row.
- The PID-5-SF and BF+M footers, which name the Society, stay in part (c) of the same row. So does the "1.0" instrument version of the APA manifest rows.

## Acceptance criteria

- [ ] AC1: Each adult form's stored APA notice is the whole notice line that its shelved APA PDF prints, compared after quotes are made straight and whitespace is collapsed. `data-raw/check_pid_adult_text.R` finds the stored notice in the PDF text, finds the PDF's notice line equal to it, and exits 0.
- [ ] AC2: The adult full and adult brief Word forms, US and A4, print their stored APA notice in the footer in place of the Society line. The PID-5-SF, BF+M, HiTOP-SR, HiTOP-BR and HiTOP-HSUM Word footers keep the Society line. A test reads the footer of each of these forms. The existing parse-back tests of the adult forms' items, instructions, legend and scoring page pass with no edit to their expectations.
- [ ] AC3: The Qualtrics file and the REDCap data dictionary of each of the five APA forms print the form's stored notice once, as the last element after the last item. In Qualtrics it is a descriptive text question, and in REDCap a descriptive field. Tests parse each export at its default arguments, with `include_instructions = FALSE` (Qualtrics), with `required = FALSE` (REDCap), with `breaks = NULL`, and with a `breaks` value that divides the form's item count (20, 5, 109, 20 and 5). Each test finds the notice once and last, and the REDCap notice field is not required.
- [ ] AC4: The other online exports print no APA notice and do not change. A test regenerates each other Qualtrics `.txt` file and finds it byte-equal to the committed file in `inst/extdata/`. It regenerates each other REDCap zip and finds its instrument CSV equal to the committed zip's. The HiTOP-HSUM Qualtrics QSF is not rebuilt.
- [ ] AC5: The rebuilt files ship with new `hitop_artifacts` rows, and each is byte-identical to its row. These are the adult full and brief Word files (US and A4) and the Qualtrics and REDCap files of the five forms. At review, `git diff --name-only main -- inst/extdata pkgdown/assets/downloads` lists only these 14 files and their 14 site copies. The JSON exports do not change.
- [ ] AC6: NEWS.md describes the notice. `devtools::test()` passes, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T3
- AC5 → T4
- AC6 → T5

## Tasks

- [ ] T1: Make sure that the adult full and brief PDFs are on the shelf. If they are missing, set the milestone `blocked` and name the PDFs. Write the references page for the two PDFs, with their sha256. Store the two notices in `data-raw/sysdata.R`, write `data-raw/check_pid_adult_text.R` (it checks each PDF's sha256 first) and add the SOURCES.md rows. Make sure that each stored notice contains "Copyright", which test-artifacts.R:59 requires of every Word footer. If one does not, stop and report it.
- [ ] T2: Pass the adult notices as `footer_notice` in `generate_docx_pid5()` and `generate_docx_pid5bf()` (R/generate_docx.R:1294, :1424). In test-generate-pid5irf.R:170-189, remove `pid5` and `pid5bf` from the Society-footer list, add `generate_docx_hitophsum`, and add the adult APA footer tests.
- [ ] T3: Give the shared online builders (`build_qualtrics_txt()`, `build_redcap_zip()`) a notice argument and pass each APA form's notice from its generators. Change the REDCap row-count expectations of the five forms from n + 1 to n + 2 (test-generate_redcap.R:81 and :97-112, test-generate-pid5irf.R:238, test-generate-pid5child.R:241). Write AC3's tests, and AC4's test as a regression lock in the D-011 harness pattern (the parse-back tests stay the IP2 oracle).
- [ ] T4: Rebuild with `data-raw/artifacts.R` in two runs, never with `json`. First run: stems `pid5` and `pid5bf`, formats docx, qualtrics and redcap. Second run: stems `pid5irf`, `pid5child` and `pid5bfchild`, formats qualtrics and redcap. Stage the site copies and make sure that the manifest rows are correct.
- [ ] T5: Write NEWS. Run `devtools::document()`, `devtools::test()` and `devtools::check()`. At the merge question, Jeff signs off the stored notices and their placement under IP1. Record that sign-off as a D-entry.

## Work log

- 2026-10-07: created by /milestone-plan, together with M170.
- 2026-10-07: question set: how close do the APA Word forms get to the APA page? Answer: ask APA first. The Word layout waits on APA's written permission, and the online notice ships now.
- 2026-10-07: question set: will Jeff add the adult APA PDFs? Answer: yes, before M171 starts. psychiatry.org returned HTTP 403 to a scripted request, so the plan did not download them.
- 2026-10-07: plan gate chose to ship the notice now and hold the Word layout (Jeff) over matching the APA page or adding identity lines only. The APA rights page bars changes without written permission. Falsified by APA's written reply, either way.
- 2026-10-07: plan chose the notice as the last element over the first, because the APA PDFs print it in the page footer, after the items. Falsified by an APA online version of the form that puts the notice first.
- 2026-10-07: M171's criteria come from audited drafts (M171 AC4 and M172 AC1 to AC4 of the 2026-10-07 draft) and apply findings 11 and 14 to 16. They went back through the audit (fresh Opus reader, full mode, together with M170's AC1), which returned 7 findings on M171. Six were applied. AC1 compares the whole notice line, and the script exits 0. T1 checks the PDF sha256 and the word "Copyright". T2 names the Society-footer test edit. AC3 probes `required = FALSE`, `breaks = NULL` and named divisors, and T3 updates the REDCap counts. T3 frames AC4's test as a regression lock. T4 rebuilds in two runs, so the informant and child Word files do not change. Not applied as a criterion: Jeff's IP1 sign-off as an AC. A D-entry is a recording act, not a property of the forms, so it stays in T5 and the merge question carries the sign-off.
