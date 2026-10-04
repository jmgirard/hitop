# M158: PID5BF+M Word, Qualtrics and REDCap forms

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M157
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — three new exported generators and their downloads
- **Branch/PR:** m158-pid5bfpm-forms

## Goal

Researchers can build and download PID5BF+M paper forms, a Qualtrics import file and a REDCap data dictionary.

## Scope

**In:** Word (US and A4), Qualtrics and REDCap generators for the BF+M, built on the existing PID-5 builders. The item order and text come from M157's `pid_items` column. The instructions and response labels enter `R/sysdata.rda`. The prebuilt files ship with manifest rows and a download page without an online-form strip.

**Out:**
- The 34-item PID5BF+ forms wait on its source (candidate row).
- Module Builder, hitop-form and Study Link Builder support get a candidate row.
- The German BF+M wording is not shipped. The package's PID-5 text is English only.

## Acceptance criteria

- [ ] AC1: The BF+M Word generator writes US and A4 forms. The parsed item rows are exactly the 36 BF+M items in BF+M order. Each row shows its BF+M number, its `pid_items` text and the 0 to 3 values. The parsed legend and instructions equal the `R/sysdata.rda` instruction object that T1 settles. The parsed scoring table equals M157's BF+M facets, item pairs and domain map, and its instruction line states M157's scoring metric. No other item appears.
- [ ] AC2: The BF+M Qualtrics file and REDCap zip each hold exactly the 36 BF+M items in BF+M order. Each item carries its `pid_items` text and the response values and labels of the same instruction object. REDCap field names follow D-055 (`<stem>_01`, with the stem the M157 gate chose). Qualtrics IDs follow the existing uppercase `<STEM>_01` convention.
- [ ] AC3: The four files (Word US, Word A4, Qualtrics, REDCap) ship in `inst/extdata/` and `pkgdown/assets/downloads/`. Each is byte-identical to the file that its `hitop_artifacts` row records. A new download page links each one and carries no online-form strip. The page counts in `test-artifacts.R` and `test-download-pages.R` and the strip test's exemption admit the new page.
- [ ] AC4: NEWS.md and the `_pkgdown.yml` reference index list the new generators. `pkgdown::check_pkgdown()` passes. `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2, T4
- AC2 → T1, T3, T4
- AC3 → T5
- AC4 → T6

## Tasks

- [x] T1: Settle the English instructions and response labels for the form. When Jeff uploads Bach et al. (2020) or Johannes Zimmermann's materials, take them from there. If neither source gives English text, put the APA PID-5 text to Jeff at the pre-implementation gate. Store the result in `R/sysdata.rda` through `data-raw/`, as a new object or a reuse of `pid_instructions`. Record the source in SOURCES.md. (RB tripwire: ip-touching)
- [ ] T2: Add the BF+M Word generator in `R/generate_docx.R`, following `generate_docx_pid5bf()`.
- [ ] T3: Add the Qualtrics and REDCap generators, following the BF ones.
- [ ] T4: Write the parse-back tests (AC1, AC2) in the D-010 style.
- [ ] T5: Add the rows to `data-raw/artifacts.R`, build the artifacts and stage the pkgdown copies. Write the download page and add it to the navbar. Update the page counts in `test-artifacts.R` and `test-download-pages.R`, and exempt the page from the online-strip test.
- [ ] T6: Add NEWS and reference rows. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-03: created by /milestone-plan, together with M157 and M159–M161.
- 2026-10-03: plan chose to take item text from the `pid_items` rows that the BF+M key names over typing a new English item list. The key maps each BF+M item to a PID-5 item. Falsified by an English BF+M source whose item wording differs from the APA PID-5 text.
- 2026-10-03: criteria audit (full mode, fresh Opus reader) returned 4 clear fixes and 1 judgment finding, all applied. AC1 binds a stored instruction object and the scoring table. AC2 separates REDCap names (D-055) from uppercase Qualtrics IDs. AC3 counts four files and exempts the page from the online-strip test.
- 2026-10-03: note from M157's RR06. `make_scoring_table()` sorts rows by `Scale` alphabetically and prints one item list per row, so it cannot show the BF+M domain map by itself. AC1's scoring table needs a planned domain presentation, such as a second table or a sentence. D-088 fixes the names, the item-mean scale and `pid_bfpm_domains`.

- 2026-10-03: implement started on branch `m158-pid5bfpm-forms`, cut from main at `226e78ba` after M157 merged. The untracked `devel/hitopdat_*` files predate the branch and stay out of every commit.
- 2026-10-03: T1 search: no English BF+M instructions are on the shelf. Bach et al. (2020) prints none. Kerber et al. (2022) and its supplement print none for the BF+M. The FU Berlin sheet's German instructions are a sentence-by-sentence translation of the stored APA PID-5 `pid_instructions$start`. Its labels "Trifft überhaupt nicht zu" to "Trifft genau zu" sit on the same 0 to 3 values as the APA labels. T1 stops at its tripwire gate.
- 2026-10-03: T1 done. Jeff chose to reuse the APA PID-5 `pid_instructions` (instructions and 0 to 3 labels) for the BF+M forms over escalation, waiting, or stopping. The tripwire escalation was offered and not taken. This is IP1 sign-off for the form text. No `R/sysdata.rda` change is needed, and SOURCES.md has a new row.
- 2026-10-03: catch-up: the previous session left T2 and T3 code uncommitted (the three generators, `make_domain_table()`, a `table_3` slot in `build_hitop_doc()`, and the `docx_domain_rows()` test helper). This session found it in the tree and commits it here.
- 2026-10-03: implement choice: the BF+M domain map prints as a second table after the facet table (Domain, "Average of these facet scores"), in `pid_bfpm_domains` order. The scoring line says to average items for a facet, then average the three facets for a domain. This answers the RR06 note.
- 2026-10-03: T4 tests written in `test-generate-pid5bfpm.R` (123 expectations pass). Planted defects (reversed item order in Word and REDCap, reversed domain rows) turned 7 of them red. The full suite has one failure: the vignette export-coverage test lists the three new generators, which T5's download page will link. T2 to T4 stay unticked until the suite is clean.

## Decisions

## Review
