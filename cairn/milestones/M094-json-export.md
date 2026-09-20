# M094: The package ships a JSON export of the HiTOP-SR and HiTOP-BR items, response options and instructions as a checksum-locked artifact

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP4
- **Resolves:** —
- **Surface tier:** user-facing — a distributed artifact under D-016 that a page outside the package reads
- **Branch/PR:** m094-json-export

## Goal

Ship one JSON file per HiTOP form holding its items, response options and administration instructions as the package's tables hold them, locked to the tables by test and to the manifest by checksum, so a page outside the package can render the instrument.

## Scope

**In:** `data-raw/json_export.R` writing `inst/extdata/hitopsr.json` and `inst/extdata/hitopbr.json`. Each file's top level: `format` (`"1.0"`), `package`, `packageVersion`, `buildDate`, `stem` (the file stem, also the item-column prefix before the underscore), `maxItem`, `instructions` (`start`, and `options` as an array of `{value, label}`), and `items` as an array of `{number, name, text}` in table order. Manifest rows and staged copies through `data-raw/artifacts.R`, a parse-and-compare test, download-page links, NEWS.

**Out:** the PID-5 forms and the HSUM → the online-form candidate row (Jeff at the 2026-09-20 plan gate: HiTOP-SR and HiTOP-BR first). Keying (reverse flags, scale membership) is not exported: the page never scores and the package rebuilds keying from its own tables (D-039). The page → M095. The package-side reader → M096.

## Acceptance criteria

- [x] AC1: For each of `hitopsr` and `hitopbr`, `inst/extdata/<stem>.json` parses with `jsonlite::fromJSON(simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)`, and its `items` array equals, element by element and in table order, the rows of `hitopsr_items` (numbered by `HSR`) or `hitopbr_items` (numbered by `HBR`) on `number` as an integer, on `name` as `item_names(paste0(stem, "_"), number, maxItem)` builds it, and on `text`; its `instructions.start` equals the matching `*_instructions$start`, and its `instructions.options`, taken as integer `value` and character `label` in array order, equals that object's `options` data frame row for row. A test in `tests/testthat/` asserts this with the expected side read from the tables and `R/sysdata.rda`, never from the file.
- [x] AC2: Each of the two files has a `hitop_artifacts` row with `format` `"json"` whose `md5` equals the committed file's, and a byte-identical copy under `pkgdown/assets/downloads/`, which a local `pkgdown::build_site()` places at `docs/downloads/<stem>.json`, the path the deployed site serves at `https://jmgirard.github.io/hitop/downloads/<stem>.json` (D-033).
- [x] AC3: Each file carries top-level `format` `"1.0"`, `package` `"hitop"`, `packageVersion` equal to the current `DESCRIPTION` Version (a mismatch is a rebuild trigger), `buildDate` equal to its current manifest row's `build_date`, `stem` equal to the file stem, and `maxItem` equal to the largest item number in its table; the AC1 test asserts these fields.
- [x] AC4: Neither committed file holds a carriage-return byte, and `Rscript data-raw/check_line_endings.R` passes.
- [x] AC5: `vignettes/articles/download-hitopsr.Rmd` and `download-hitopbr.Rmd` each carry a JSON card linking the form's file at `../downloads/<stem>.json`, described in one sentence addressed to the researcher that opens with an imperative verb as the page's other download cards do, names the three things the file holds (its items, response options and instructions) and says that a web form outside the package can read it; `NEWS.md` names both files; `devtools::test()` and `devtools::check()` are clean.

## Coverage

- AC1 → T1, T3
- AC2 → T2, T4
- AC3 → T1, T3
- AC4 → T1, T4
- AC5 → T4, T5

## Tasks

- [x] T1: Write `data-raw/json_export.R`: build each payload from `hitopsr_items`/`hitopbr_items`, the matching `*_instructions` object (`data-raw/sysdata.R:24-41`) and `item_names()` (`R/util.R:697`); serialize with `jsonlite::toJSON(pretty = TRUE)` with scalars `unbox`ed as `R/module_file.R:130-140` does; write through `file(path, open = "wb")` with `useBytes = TRUE` (LESSONS 2026-07-16).
- [x] T2: Register both files in `data-raw/artifacts.R` (`add_row()` at :201, staging at :280) with `format = "json"`; run it; regenerate `hitop_artifacts`.
- [x] T3: `tests/testthat/test-json-export.R`: the AC1 and AC3 assertions with the expected side from the tables; before trusting green, plant in a temporary copy, one at a time, a changed item text, a dropped item, two swapped items, a changed option label, a changed `instructions.start` and a changed `stem`, and see the test red on each.
- [x] T4: Download articles and NEWS; run `data-raw/check_line_endings.R`; `devtools::document()`; `devtools::test()`; `devtools::check()`; `pkgdown::build_site()` and confirm `docs/downloads/<stem>.json` for both stems.
- [x] T5: Execute the AC5 amendment (one-sentence imperative JSON card on both download pages) and the review's confirmed fix-now findings: script-mode load order and header claim in `data-raw/json_export.R`; the manifest `changes` note naming the hitop-form page (json rows regenerated through `artifacts.R`); `"json"` in `hitop_artifacts`' roxygen; the stale staged-file count comment; DESIGN.md's generator list and artifact-versioning paragraph; NEWS naming the site as the address.

## Work log

- 2026-09-20: created by /milestone-plan.
- 2026-09-20: criteria audit ran in full mode on a fresh [O] reader over the six-form draft: 33 findings across the four drafted milestones, 30 fixed at the gate (named instruction and items tables, `stem` versus prefix, `packageVersion` against the current DESCRIPTION, instrument clauses removed, plants per field family), three posed as gate questions.
- 2026-09-20: plan gate chose HiTOP-SR and HiTOP-BR only over all six forms (Jeff's call); the PID-5 and HSUM exports live in the candidate row; falsified by nothing, a scope choice.
- 2026-09-20: the audit's second pass over the final wording returned six findings across the three files, all fixed before implementation: here AC2 names the served path and T3 plants `instructions.start`.
- 2026-09-20: plan chose exporting the tables without keying over a self-contained descriptor with reverse flags and scale membership because the page never scores and D-039 rebuilds keying from the package; falsified by a page needing to score in the browser.
- 2026-09-20: implement started on `m094-json-export`; the pre-implementation gate was skipped because the plan left no API, naming or dependency choice open (jsonlite is already an Import).
- 2026-09-20: T1 done: `data-raw/json_export.R` writes both files through a binary connection; `hitopsr.json` 51,477 bytes, `hitopbr.json` 6,255 bytes.
- 2026-09-20: T2 done: `artifacts.R` gains `json_specs`, a `json` format and the two manifest rows (41 rows, the prior 39 unchanged); `test-artifacts.R` admits `json` in its format vocabulary and file pattern.
- 2026-09-20: T3 done: `test-json-export.R` reports every disagreement by field name; six plants (text, dropped item, swapped items, option label, instructions start, stem) each red under their own name; 222 passes across the export, artifact and staged-copy files.
- 2026-09-20: T4 done: a JSON card on both download pages, a `json` format label in `_download-helpers.R`, a NEWS entry; `check_line_endings.R` passes; `document()` no diff; `devtools::check()` 0 errors, 0 warnings, 0 notes in 4m 12s; `pkgdown::build_site(install = TRUE)` places both files at `docs/downloads/` byte-identical (with `install = FALSE` the page helpers read the installed package's older manifest and the render fails, which CI's install step avoids).
- 2026-09-20: claim audit: 34 claims read, 2 corrected — data-raw/json_export.R (sourcing versus running as a script), data-raw/artifacts.R, NEWS.md, vignettes/articles/download-hitopsr.Rmd, download-hitopbr.Rmd (a hitop-form page named as an existing consumer, reworded to a capability); re-read once, 0 wrong.
- 2026-09-20: all tasks done, status set to review.
- 2026-09-20: amendment return: AC5 — "`vignettes/articles/download-hitopsr.Rmd` and `download-hitopbr.Rmd` each link the form's JSON file with card text saying what it holds and that a web form outside the package can read it to show the questionnaire; `NEWS.md` names the two artifacts; `devtools::test()` and `devtools::check()` are clean." — the shipped card is two sentences and names no hitop-form page because that page does not exist until M095 (the implement claim audit reworded it deliberately); the criterion as written demands a false claim, so the criterion is wrong, not the work. Re-audit and mini gate via /milestone-implement, then re-review.
- 2026-09-20: re-audit: AC5 (full) — four findings on the proposed wording: "card text" unbounded and fitted to the shipped card; the count drop also drops the page's imperative voice; the clause checks that the card says something, not that it is true; the link clause is route-agnostic where D-033 makes `../downloads/<stem>.json` canonical. Reader proposed a one-imperative-sentence wording pinning the route; posed at the mini gate.
- 2026-09-20: mini gate (Jeff): the reader's one-sentence imperative wording adopted over the review's "card text" wording; the review's confirmed fix-now findings (F1, F4, F5, F11, F12, F15, F18) taken onto the branch now rather than at the re-review gate.
- 2026-09-20: re-audit: AC5 (full) — once re-entry on the gate wording: four findings (level of detail unfixed, claim-presence over truth, a moving comparator, a route clause duplicating `test-artifacts.R`'s href lock); reader's tightened wording adopted by Jeff; AC5's re-entry is spent, no further reader.
- 2026-09-20: T5 done: both JSON cards read "Use this machine-readable file to drive a web form outside the package with the instrument's items, response options and instructions as the package's tables hold them."; `json_export.R` loads the package before `json_specs` (script mode now writes files identical to the committed ones) and its header states the unconditional source; the two json manifest rows regenerated through `artifacts.R` after dropping them, same md5 and date, the note no longer naming the hitop-form page; `"json"` added to `hitop_artifacts`' roxygen (`document()` rewrote `man/hitop_artifacts.Rd`); staged-file count comment 24 → 26; DESIGN.md generator list and artifact-versioning paragraph name the JSON export; NEWS names the site download as the address. `check_line_endings.R` passes; `devtools::test()` 0 failures, 17,399 passes.
- 2026-09-20: claim audit: not re-run — the milestone's one pass stands (stopping rule); the amendment's added prose (card, NEWS, script header) was read against the code by the two fresh AC5 readers and by running script mode on a scratch copy.
- 2026-09-20: `devtools::check()` after T5: 0 errors, 0 warnings, 0 notes; all tasks done, status set to review.

## Decisions

## Review

- 2026-09-20 (review): main unmoved since the branch was cut (merge-base equals `origin/main`), no PR exists for the branch; review runs on the local branch.
- AC1 evidence: `devtools::test(filter = "json-export|artifacts")` 0 failures, 138 passes; `test-json-export.R` parses each file with the criterion's `fromJSON` call and compares `items` (number as integer, name via `item_names()`, text), `instructions.start` and `instructions.options` (integer value, character label, row for row) against `hitopsr_items`/`hitopbr_items` and `hitop:::*_instructions`; the file supplies no expected value. Six plants each red under their own field name.
- AC3 evidence: both files read `format` `"1.0"`, `package` `"hitop"`, `packageVersion` `"0.2.0"` (DESCRIPTION Version 0.2.0), `buildDate` `"2026-09-20"` equal to the json manifest rows' `build_date`, `stem` `hitopsr`/`hitopbr`, `maxItem` 405/45; the same test asserts each field.
- AC4 evidence: `grep -c $'\r'` returns 0 on all four committed JSON files; `Rscript data-raw/check_line_endings.R` passes.

- AC2 evidence: `hitop_artifacts` json rows carry md5 `1e438cf9…` (hitopsr) and `4768019b…` (hitopbr), equal to `md5 -q` on the committed files; `cmp` finds `pkgdown/assets/downloads/<stem>.json` byte-identical to `inst/extdata/`; `pkgdown::build_site(install = TRUE)` exits 0 and places both at `docs/downloads/<stem>.json`, byte-identical, with each download page linking its file; deployed path per D-033 is `downloads/<stem>.json` under the site root.
- AC5 evidence (partial, box left unticked): `NEWS.md` names `hitopsr.json` and `hitopbr.json`; `devtools::test()` 0 failures, 17,399 passes; `devtools::check()` 0 errors, 0 warnings, 0 notes; `devtools::document()` no diff; `pkgdown::check_pkgdown()` clean; README.Rmd and README.md last changed in the same commit. Fails as written: the card text is two sentences and says "A web form outside the package can read it", not that it is what the hitop-form page reads. Amendment return recorded in the work log.
- Consistency gate: `cairn_validate.py` exit 0 (24 advisory warnings, all pre-existing dangling D-ids and one references-staleness line); no DESIGN principle changed, so `cairn_impact` skipped; toolchain slot: document no diff, check clean, check_pkgdown clean, NEWS entry present without milestone numbers, no new top-level files.
- Independent review, three lenses, 2026-09-20. Dispositions pending triage at the re-review gate; the fix-now candidates are marked.
  - [O] F1 `data-raw/json_export.R:32,82` — `json_specs` reads `hitopsr_items` at parse time, so `Rscript data-raw/json_export.R` halts with "object 'hitopsr_items' not found" before `load_all()` runs; the header's script-mode claim is false. Reproduced. Fix-now candidate.
  - [O] F2 `data-raw/json_export.R:70` — `name` is `hitopsr_001`/`hitopbr_01` (the stem), while the package's item prefix is `hsr_`/`hbr_` everywhere else; a page writing columns under these names misses `score_hitopsr()`'s default prefix. Plan choice (AC1/Scope name the stem), not a slip; flag for M095/M096 planning.
  - [O] F3 `test-json-export.R:53` — a DESCRIPTION version bump reds the suite until `artifacts.R` reruns; intended by AC3 but unrecorded in the release walk. Follow-up candidate.
  - [O] F4 `data-raw/artifacts.R:128` — `build_notes` "read by the hitop-form page" ships in `hitop_artifacts$changes` and renders in the Versions history; the claim audit missed this copy. Confirmed. Fix-now candidate (needs a manifest regeneration).
  - [O] F5 = [S-prior] F1 `R/data.R:379` — `hitop_artifacts$format` roxygen omits `"json"`. Confirmed. Fix-now candidate.
  - [O] F6 `json_export.R:57` — `Sys.Date()` makes an otherwise byte-reproducible file churn on rerun. Design note; follow-up candidate.
  - [O] F7 `test-json-export.R:47,80` — `vapply(..., integer(1))` errors on a double, so the `number.type` note can never be the reported result and no plant exercises it. Test-quality; follow-up candidate.
  - [O] F8 `test-json-export.R:124` — six plants share one tempfile path. Follow-up candidate.
  - [O] F9 — plants re-serialize with `auto_unbox = TRUE`, so the writer's length-one-array guarantee is untested. Follow-up candidate.
  - [O] F10 — `manifest_build_date()` duplicates `latest_manifest()` rather than sharing a helper. Style; reject or follow-up.
  - [O] F11 `json_export.R:10` — "sourced under its rebuild filters" is wrong: the source is unconditional, only the write loop is filtered. Fix-now candidate (comment).
  - [O] F12 `cairn/DESIGN.md:46,84` — generator list and artifact-versioning paragraph omit the JSON format. Fix-now candidate (docs).
  - [O] F13 — the card breaks the page's imperative voice and AC5's wording. The AC5 half is the amendment return above; the voice half is a wording choice for the mini gate.
  - [O] F14 — four `col-md-4` cards wrap one onto a second row on these two pages. Cosmetic; reject or follow-up.
  - [O] F15 `test-artifacts.R:203` — comment says 24 staged files, now 26. Confirmed. Fix-now candidate (comment).
  - [O] F16 — no direct test of `write_instrument_json()`; two cross-artifact sweeps filter to qualtrics/redcap. Lost redundancy; follow-up candidate.
  - [O] F17 — the export's top level is shaped like the D-039 module descriptor with the same `format` `"1.0"` and no discriminator. Flag for M096's reader; follow-up candidate.
  - [O] F18 — NEWS names `inst/extdata/` as the address where D-033 made the site canonical. Fix-now candidate (wording).
  - [S-blame] no conflicts with past commits or D-entries; `rebuild_stems`/`rebuild_formats` left as the last build ran them matches every prior commit; the writer matches `write_module()` and the CRLF lesson.
  - [S-prior] probe `pulls/comments?per_page=1` returned `[]`; archived reviews of M020, M042, M047, M054 held up except the `R/data.R` format enumeration (F5 above).
- 2026-09-20 (review, pass 2 after the AC5 amendment and T5): main still unmoved (merge-base equals `origin/main`), no PR; the branch is 9 commits ahead.
- AC1 evidence (pass 2): `devtools::test()` 0 failures, 17,399 passes on the T5 head; `test-json-export.R` unchanged since pass 1 (parse with the criterion's `fromJSON` call, compare items/start/options against the tables and `hitop:::*_instructions`, six plants red by name).
- AC3 evidence (pass 2): both files still read `format` `"1.0"`, `package` `"hitop"`, `packageVersion` `"0.2.0"` (DESCRIPTION 0.2.0), `buildDate` `"2026-09-20"` equal to the regenerated json rows' `build_date`, `stem` `hitopsr`/`hitopbr`, `maxItem` 405/45; the files are byte-unchanged since pass 1 (md5 `1e438cf9…`, `4768019b…`).
- AC4 evidence (pass 2): 0 carriage-return bytes in all four committed JSON files; `check_line_endings.R` passes.
- AC2 evidence (pass 2): json manifest rows md5 `1e438cf9…`/`4768019b…` equal `md5 -q` on the committed files; `cmp` finds the `pkgdown/assets/downloads/` copies byte-identical; `pkgdown::build_site(install = TRUE)` exits 0 and places both at `docs/downloads/<stem>.json`, byte-identical, each linked from its download page (D-033 route `downloads/<stem>.json` under the site root).
- AC5 evidence (pass 2): both download pages carry a JSON card whose link is `../downloads/hitopsr.json` / `../downloads/hitopbr.json` and whose text is one sentence, "Use this machine-readable file to drive a web form outside the package with the instrument's items, response options and instructions as the package's tables hold them." (opens with the imperative "Use" as the other download cards do, names items, response options and instructions, says a web form outside the package can use it); `NEWS.md` names `hitopsr.json` and `hitopbr.json`; `devtools::test()` 0 failures, 17,399 passes; `devtools::check()` 0 errors, 0 warnings, 0 notes.
- Consistency gate (pass 2): `cairn_validate.py` exit 0, release window quiet; no principle changed (`cairn_impact` skipped); `document()` no diff; `check_pkgdown()` clean; README in sync; NEWS entry present, no milestone numbers; check clean with no new NOTE.
