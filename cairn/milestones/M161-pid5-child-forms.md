# M161: PID-5 child forms (ages 11 to 17)

- **Status:** review
- **Priority:** normal
- **Depends on:** M158
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3
- **Resolves:** —
- **Surface tier:** user-facing — new exported form generators, downloads and scoring documentation
- **Branch/PR:** m161-pid5-child-forms

## Goal

Researchers can build, download and score the APA PID-5 child forms for ages 11 to 17 (220-item full form, 25-item brief form).

## Scope

**In:**
- The child instructions enter `R/sysdata.rda`. The sources are the two APA child forms on the shelf (`apa2013pid5child.pdf`, `apa2013pid5bfchild.pdf`).
- Word, Qualtrics and REDCap child forms are built on the existing PID-5 builders, with manifest rows and download pages.
- The help pages state that the existing FULL and BF versions score the child forms.

**Out:**
- If T1 finds that the child item text, item order or keying differ from the adult form, those differences need a keying change. They return to the plan through the amendment protocol. They are not absorbed here.
- Child norms are not on the shelf, so they get no row.
- The parent-report BF+M for children (Mazreku et al., 2023) has no public item list or key, so it gets no row. The forensic form and the PID5BF+ have their own candidate row.
- Module Builder, hitop-form and Study Link Builder support stay on the downstream candidate row.

## Acceptance criteria

- [ ] AC1: A references page records whether the child forms share the adult forms' items and keying. It reports a comparison run by a `data-raw/` script on the shelf copies. The script checks the 220 item texts and the 25 BF item texts in order against `pid_items$Text`. Texts match exactly after whitespace and typographic quotes are normalized and a final period is dropped from each text, because `pid_items$Text` stores none. It also checks the child reverse list, facet table and domain tables against `pid_items$Reverse`, `pid_scales$FULL`, `pid_scales$BF` and `pid_domains`. The page names the script and its run date, and it lists every difference found, or none.
- [x] AC2: The child Word forms (full and BF, US and A4) parse back to exactly the `pid_items` FULL and BF items in order. Each form carries the child instructions and response labels transcribed from the APA child forms. The child Qualtrics files and REDCap zips parse back to the same items, instructions and labels. The tests are in the D-010 style.
- [ ] AC3: The shipped adult artifacts do not change. `hitop_artifacts` gains only the 8 child rows, and every row on main is unchanged. No file under `inst/extdata/` or `pkgdown/assets/downloads/` that exists on main differs from main's copy. Every test file on main that calls a `generate_*()` function passes with no edit to its expectations, except that `test-export-padding-width.R`'s two export counts change from 15 to 19 and `test-response-value-no-move.R`'s builder list gains the four child online exports' builders. No other line of those two files changes. AC4 governs `test-artifacts.R` and `test-download-pages.R`. The adult generators' signatures do not change.
- [x] AC4: The child files ship in `inst/extdata/` and `pkgdown/assets/downloads/`. Each is byte-identical to the file that its `hitop_artifacts` row records. Download pages link each one and carry no online-form strip. The page counts in `test-artifacts.R` and `test-download-pages.R` and the strip test's exemption admit the new pages. Those are the only edits to the expectations of those two files.
- [x] AC5: The `score_pid5()` help page and the scoring vignette say that `version = "FULL"` or `"BF"` scores the child forms. Both cite the APA child forms. NEWS.md and the `_pkgdown.yml` reference index list the new generators. `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T1, T7
- AC2 → T2, T3, T4
- AC3 → T4, T7
- AC4 → T5
- AC5 → T6

## Tasks

- [x] T1: Write the `data-raw/` comparison script and the references page (AC1). A first check on 2026-10-03 found all 220 adult texts and all 25 BF texts in the child PDFs (letters-only match, order not checked). The child reverse list equals the adult list.
- [x] T2: Pre-implementation gate (RB tripwire: irreversible-api). Choose between new child generator functions and an argument on the existing PID-5 generators. Also choose the child item-name stems for Qualtrics and REDCap. Those stems decide whether `rename_pid5_items()` is needed before FULL or BF scoring. Then add the child instructions to `R/sysdata.rda` through `data-raw/`.
- [x] T3: Build the child Word, Qualtrics and REDCap generators.
- [x] T4: Write the parse-back tests (AC2) and run the existing generator and artifact tests unchanged (AC3).
- [x] T5: Add the artifact rows, build the artifacts, stage the pkgdown copies and write the download pages. Update the page counts in `test-artifacts.R` and `test-download-pages.R`, and exempt the pages from the online-strip test.
- [x] T6: Update the help pages, vignette, NEWS and reference index. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.
- [x] T7: Apply the AC1 and AC3 amendment returns from review pass 1: re-audit the amended wording, and tighten `data-raw/check_pid_child_text.R` as the AC1 audit found.

## Work log

- 2026-10-03: created by /milestone-plan, together with M157 to M160. It depends on M158 so that the generator changes do not collide.
- 2026-10-03: plan chose to score the child forms with the existing FULL and BF versions over a new child version. The first text check found the same items and reverse list. Falsified by any difference in T1's comparison.
- 2026-10-03: criteria audit (full mode, fresh Opus reader) returned 3 clear fixes and 3 judgment findings, all applied. AC1 compares against the package tables, reads "whether", and states the match rule. AC3 narrows to shipped adult artifacts and stops conflicting with AC4's page-count edits. The T2 gate adds the child item-name stems.
- 2026-10-04: implement started on branch `m161-pid5-child-forms`, cut from main at `0b9bc3db` after M160 merged. The untracked `devel/hitopdat_*` files predate the branch and stay out of every commit. M160's `pid_irf_form()`, `page_header` and `footer_notice` patterns are available to the child generators.
- 2026-10-04: T1 done. `data-raw/check_pid_child_text.R` finds no difference in the 220 + 25 texts, the reverse list, 25 facets, 5 domains or 5 BF domains, and goes red on 7 planted defect kinds. Page `cairn/references/apa2013pid5child.md`. The child instructions differ from `pid_instructions$start` only in the full form's label and its quotes around "right" and "wrong".
- 2026-10-04: T2 gate (RB tripwire: irreversible-api) posed as one chip with the escalation offer. Jeff chose new child functions and the adult item names, recorded as M161-D1. `pid_child_instructions` (FULL and BF: start, notice, options) enters `R/sysdata.rda` via `data-raw/sysdata.R`. The check script gains a sixth check on it, which passes and goes red on 3 planted defects. Suite: 27632 pass, 1 fail, `test-vignette-export-coverage.R`, caused by T3's new exports reaching `NAMESPACE` mid-run. It clears when T5 links them.
- 2026-10-04: T3 and T4 checkpoint, boxes left open until T5 makes the suite green. Six generators share `pid_child_form()` and `build_docx_pid5_child()`. Default titles are "PID-5 (Full), Child Age 11–17" and "PID-5-BF, Child Age 11–17". Qualtrics blocks are "PID-5 Child" and "PID-5-BF Child", and REDCap forms are `pid5child_questionnaire` and `pid5bfchild_questionnaire`. `test-generate-pid5child.R` passes, 6 tests. Planting adult instructions or two swapped items fails 3 of them.
- 2026-10-04: T5 done, T3 and T4 boxes ticked. `data-raw/artifacts.R` built the 8 child files, and the manifest gained 8 rows, all child files. All 52 earlier rows are unchanged and no shipped adult file changed. Pages `download-pid5child.Rmd` and `download-pid5bfchild.Rmd` and two Instruments menu entries were added. Page counts went from 8 to 10, and the strip exemption gained `pid5child` and `pid5bfchild`. Three manifest-coverage tests failed on the 4 new online exports, as their comments say they will. Fix: `test-export-padding-width.R` counts 15 to 19 and `test-response-value-no-move.R` gains 4 builders. These are not generator tests under AC3, and no expectation about an adult export changed. The full suite then had 27823 pass and 3 fail, all 3 in those files. The rerun of those files and the page tests passes.
- 2026-10-04: T6 done. Added a `score_pid5()` details section and reference, a scoring vignette section, a NEWS entry and 6 reference-index entries. `pkgdown::check_pkgdown()` reports no problems. `devtools::check()` reports 0 errors, 0 warnings and 0 notes; it ran before the claim-audit fixes, and a rerun on the final state follows.
- claim audit: 35 claims read, 8 corrected — R/score_pid5.R, vignettes/pid5_scoring.Rmd, vignettes/articles/download-pid5child.Rmd, vignettes/articles/download-pid5bfchild.Rmd, data-raw/artifacts.R, data-raw/check_pid_child_text.R, data-raw/sysdata.R (plus the stale `build_docx_footer()` comment in R/generate_docx.R). The same reader re-read all corrections and found them correct. The fix to the `build_notes` wording restored `data/hitop_artifacts.rda` from main and reran `artifacts.R`, so the 8 child rows and files were rebuilt. The 52 adult rows are unchanged.
- 2026-10-04: status set to review. `devtools::check()` on the final state (98c13b6f) reports 0 errors, 0 warnings and 0 notes.
- 2026-10-04: amendment return: AC1 — "Texts match exactly after whitespace and typographic quotes are normalized and a final period is dropped, because `pid_items$Text` stores none."
- 2026-10-04: amendment return: AC3 — "The existing generator tests pass with no edit to their expectations, except that the two tests that enumerate every shipped online export (`test-export-padding-width.R` and `test-response-value-no-move.R`) gain the four child online exports and nothing else."
- 2026-10-04: return from review: amendment returns on AC1 and AC3 become T7. Status was already set to in-progress by review.
- re-audit: AC1 (full) — first reader: 4 findings. The script read `data-raw/pid_items.csv`, not the dataset. The domain check was not bound to each domain's row. BF reverse flags were unchecked. "A final period is dropped" did not say from which texts. Fixed: the script loads `data/pid_items.rda`, binds each domain to its row and checks that no BF item is reversed, with 4 new plants red. The swapped-domain plant passed the old script. The wording now says "from each text".
- re-audit: AC1 (full) — second reader: nothing on the criterion. Two page sentences were aligned with it ("from each text", and the provenance line naming both keys). AC1's amended wording is written.
- re-audit: AC3 (full) — first reader: 4 findings. Two counts rising is not "gaining" an export. "Existing generator tests" named no procedure, so the domain was undefined. The T2 conditional is dead under M161-D1. "The two tests" should read "test files". Wording tightened.
- re-audit: AC3 (full) — second reader: D-016 keeps every build's row, so "no existing checksum changes" cannot fail when an adult file is rebuilt. Also, `test-artifacts.R` names `generate_redcap_` inside a regex, so grep counts it as a generator test. This is the second re-audit on AC3, which is a stop. AC3 is left at its planned wording until Jeff chooses.
- 2026-10-04: AC3 stop resolved: Jeff chose the proposed wording (files on main byte-identical, manifest gains only the 8 child rows, the two allowed test edits named, adult signatures unchanged). It is written as AC3, and T7 is done.
- 2026-10-04: status set to review after T7. The code is unchanged since review pass 1's `check()` (0/0/0) apart from `data-raw/check_pid_child_text.R`, which is not in the build. That script exits 0 with no differences.

## Decisions

- M161-D1 (2026-10-04, T2 gate, Jeff's selection): The child forms get six new exported generators, `generate_{docx,qualtrics,redcap}_pid5child()` and `generate_{docx,qualtrics,redcap}_pid5bfchild()`. The adult generators get no new argument and do not change. The child Qualtrics and REDCap item names reuse the adult stems (`PID5_001` and `pid5_001`, `PID5BF_01` and `pid5bf_01`). A child export then scores, renames and labels with `version = "FULL"` or `"BF"` as an adult export does, with no `rename_pid5_items()` step. Cost accepted: one REDCap project cannot hold the adult and the child instrument of one form, because REDCap field names must be unique. The REDCap form names and Qualtrics block names are the child forms' own.

## Review

Pass 1 (2026-10-04), branch head 38b19117, main not moved since the cut (0b9bc3db).

- AC1: not met as written. `Rscript data-raw/check_pid_child_text.R` exits 0 with "NO DIFFERENCES" for the 220 + 25 texts, the reverse list, 25 facets, 5 domains, 5 BF domains and both forms' instructions. `cairn/references/apa2013pid5child.md` names the script and its run date and lists "Differences found: none". But the script also drops a final period from each PDF text, and AC1's match rule names only whitespace and typographic quotes. Every child text ends in a period that `pid_items$Text` does not store, so the texts do not match under the rule as written. This is an amendment return, not a defect.
- AC2: met. `test-generate-pid5child.R` parses fresh builds: Word in US and A4, Qualtrics and REDCap, both forms. It compares them with `pid_items` and with instructions, notice and labels typed from the PDFs, in the D-010 style. A separate parse of the 8 shipped files in `inst/extdata/` found items in order, the stored child instructions, labels and APA notice all TRUE for every file.
- AC3: not met as written. All 37 files in main's manifest keep their md5 on the branch, and the only added files are the 8 child files. `test-generate_docx.R`, `test-generate_qualtrics.R`, `test-generate_redcap.R`, `test-generate-pid5irf.R` and `test-generate-pid5bfpm.R` are unedited, and T2 added no argument to an adult generator. But `test-export-padding-width.R`, which exercises the generators, had two expectations changed from 15 to 19. `test-response-value-no-move.R`, which rebuilds every flat-text export, gained 4 builders. AC3 says the existing generator tests pass with no edit to their expectations. This is an amendment return: no expectation about an adult export changed.
- AC4: met. The md5 of all 8 child files in `inst/extdata/` and `pkgdown/assets/downloads/` matches their latest `hitop_artifacts` row. `download-pid5child.Rmd` and `download-pid5bfchild.Rmd` link their 4 files each and have no online strip. The diffs of `test-artifacts.R` and `test-download-pages.R` touch only the three 8-to-10 page counts, `no_strip_stems` and its comments.
- AC5: met. `man/score_pid5.Rd` has the subsection "The PID-5 child forms": `version = "FULL"` or `"BF"` scores the child forms, citing Krueger et al. (2013), with a reference to both APA child forms. `vignettes/pid5_scoring.Rmd` has the section "The PID-5 Child Forms" with the same statement and full citation. NEWS.md names all 6 new generators, and `_pkgdown.yml` lists each one once. `devtools::check()` on 05ab08fb's code reports 0 errors, 0 warnings and 0 notes, and `pkgdown::check_pkgdown()` reports no problems.
- Consistency gate: `cairn_validate.py` exits 0 with 32 advisory WARNs, none from this branch's records except a stray D-010 token warning on the AC2 line. `devtools::document()` leaves no diff. The branch does not touch README.Rmd. NEWS.md has the entry, and `check()` has 0 NOTEs.
