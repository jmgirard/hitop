# M161: PID-5 child forms (ages 11 to 17)

- **Status:** in-progress
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

- [ ] AC1: A references page records whether the child forms share the adult forms' items and keying. It reports a comparison run by a `data-raw/` script on the shelf copies. The script checks the 220 item texts and the 25 BF item texts in order against `pid_items$Text`. Texts match exactly after whitespace and typographic quotes are normalized. It also checks the child reverse list, facet table and domain tables against `pid_items$Reverse`, `pid_scales$FULL`, `pid_scales$BF` and `pid_domains`. The page names the script and its run date, and it lists every difference found, or none.
- [ ] AC2: The child Word forms (full and BF, US and A4) parse back to exactly the `pid_items` FULL and BF items in order. Each form carries the child instructions and response labels transcribed from the APA child forms. The child Qualtrics files and REDCap zips parse back to the same items, instructions and labels. The tests are in the D-010 style.
- [ ] AC3: The shipped adult artifacts do not change. No existing `hitop_artifacts` checksum changes on this branch. The existing generator tests pass with no edit to their expectations. If T2 adds an argument to the existing generators, a parse-back test also shows that each adult generator's output with non-default arguments is unchanged.
- [ ] AC4: The child files ship in `inst/extdata/` and `pkgdown/assets/downloads/`. Each is byte-identical to the file that its `hitop_artifacts` row records. Download pages link each one and carry no online-form strip. The page counts in `test-artifacts.R` and `test-download-pages.R` and the strip test's exemption admit the new pages. Those are the only edits to the expectations of those two files.
- [ ] AC5: The `score_pid5()` help page and the scoring vignette say that `version = "FULL"` or `"BF"` scores the child forms. Both cite the APA child forms. NEWS.md and the `_pkgdown.yml` reference index list the new generators. `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T1
- AC2 → T2, T3, T4
- AC3 → T4
- AC4 → T5
- AC5 → T6

## Tasks

- [x] T1: Write the `data-raw/` comparison script and the references page (AC1). A first check on 2026-10-03 found all 220 adult texts and all 25 BF texts in the child PDFs (letters-only match, order not checked). The child reverse list equals the adult list.
- [ ] T2: Pre-implementation gate (RB tripwire: irreversible-api). Choose between new child generator functions and an argument on the existing PID-5 generators. Also choose the child item-name stems for Qualtrics and REDCap. Those stems decide whether `rename_pid5_items()` is needed before FULL or BF scoring. Then add the child instructions to `R/sysdata.rda` through `data-raw/`.
- [ ] T3: Build the child Word, Qualtrics and REDCap generators.
- [ ] T4: Write the parse-back tests (AC2) and run the existing generator and artifact tests unchanged (AC3).
- [ ] T5: Add the artifact rows, build the artifacts, stage the pkgdown copies and write the download pages. Update the page counts in `test-artifacts.R` and `test-download-pages.R`, and exempt the pages from the online-strip test.
- [ ] T6: Update the help pages, vignette, NEWS and reference index. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-03: created by /milestone-plan, together with M157 to M160. It depends on M158 so that the generator changes do not collide.
- 2026-10-03: plan chose to score the child forms with the existing FULL and BF versions over a new child version. The first text check found the same items and reverse list. Falsified by any difference in T1's comparison.
- 2026-10-03: criteria audit (full mode, fresh Opus reader) returned 3 clear fixes and 3 judgment findings, all applied. AC1 compares against the package tables, reads "whether", and states the match rule. AC3 narrows to shipped adult artifacts and stops conflicting with AC4's page-count edits. The T2 gate adds the child item-name stems.
- 2026-10-04: implement started on branch `m161-pid5-child-forms`, cut from main at `0b9bc3db` after M160 merged. The untracked `devel/hitopdat_*` files predate the branch and stay out of every commit. M160's `pid_irf_form()`, `page_header` and `footer_notice` patterns are available to the child generators.
- 2026-10-04: T1 done. `data-raw/check_pid_child_text.R` finds no difference in the 220 + 25 texts, the reverse list, 25 facets, 5 domains or 5 BF domains, and goes red on 7 planted defect kinds. Page `cairn/references/apa2013pid5child.md`. The child instructions differ from `pid_instructions$start` only in the full form's label and its quotes around "right" and "wrong".

## Decisions

## Review
