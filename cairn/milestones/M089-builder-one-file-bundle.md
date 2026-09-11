# M089: One download per builder build — a zip bundle holding the questionnaire, the scoring file and a README

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3 (page behavior under D-038; no instrument content moves)
- **Resolves:** —
- **Surface tier:** user-facing — the public builder page at jmgirard.github.io/hitop-builder and the files it hands visitors
- **Branch/PR:** `m089-builder-one-file-bundle` (this repo, tracking); `m089-one-file-bundle` in jmgirard/hitop-builder (code)

## Goal

A visitor to the HiTOP-SR Module Builder clicks once per build and receives one file, a zip bundle carrying the questionnaire, its scoring descriptor and a README saying what to do with each.

## Scope

**In:** In `jmgirard/hitop-builder`, `download()` zips the questionnaire, the descriptor and a page-authored `README.txt` into one archive with the package's `{zip}` inside webR (`zip::zip(mode = "cherry-pick")`, the call the REDCap generator already makes there) and saves that one file. The bundle takes M063's stem — `<stem>.zip`; inside it the Word and Qualtrics questionnaires keep `<stem>.docx` / `<stem>.txt`, the REDCap upload archive becomes `<stem>-upload.zip`, the descriptor stays `<stem>.json`. `README.txt` names the file to field, says to keep the `.json` with the responses and read it with `read_module()`, and in the REDCap bundles says to upload the inner zip unextracted. The descriptor handover — `#saveDescriptor`, `#handover`, `#handoverLive`, `offerDescriptor()`, `takeDescriptor()` — and the `#renameNote` paragraph are deleted; `#descriptorNote` and `#downloadHint` are rewritten for one file. `README.md`'s *What the page shows*, *The scoring file* and *What the downloads are named* sections follow. `tests/smoke.spec.js` reads inside the bundle and `tests/plants.mjs` keeps every assertion planted. In this repo: tracking only.

**Out:** the step flow and folding the settings → M090; the `hitop` package is untouched (no bundling argument on the generators — the page is the only caller wanting one); the descriptor inside the REDCap upload archive itself (changes a package artifact under D-016's manifest lock; rejected below); a JS zip writer (rejected below); Windows Explorer extraction is documented, not verified (no Windows host in reach).

## Acceptance criteria

- [x] AC1: For each of the eight builds the README's naming table enumerates (format × whole-or-selection × shuffle), driven on the page with the same inputs, the browser is asked to save exactly one file, named `<stem>.zip`, whose entry list read out of the captured bundle is exactly three names: the questionnaire (`<stem>.docx`, `<stem>.txt`, or for REDCap `<stem>-upload.zip`), `<stem>.json`, and `README.txt`.
- [x] AC2: For each of those eight builds, the questionnaire and `.json` entries inside the bundle carry what the same inputs produced on the deployed pre-milestone page, captured in the same session: the `.txt` entries byte-identical; the `.json` entries identical on every field but `buildDate` and, for shuffled builds, `itemOrder`; the `.docx` entries identical on the parsed header and, for unshuffled builds, the parsed item rows, and for shuffled builds on the item set and on whether a crosswalk table is present; the REDCap inner zip's `instrument.csv` byte-identical.
- [x] AC3: `README.txt` read out of each of the eight captured bundles names the questionnaire entry as the file to field, says the `.json` entry is kept with the collected responses and read by `read_module()`, and — in the two REDCap bundles and no other — says the inner `-upload.zip` is uploaded to REDCap without being extracted.
- [x] AC4: Extracting each of the two captured REDCap bundles with `unzip` and with macOS `ditto -x -k` leaves `<stem>-upload.zip` on disk as a zip whose entry list is unchanged from the inner zip the page built.
- [x] AC5: A DOM query of the booted page for `#saveDescriptor`, `#handover`, `#handoverLive` and `#renameNote` returns no element, and the download step's `.downloadrow` holds exactly one button, `#downloadBtn`.
- [ ] AC6: `tests/smoke.spec.js`'s download assertions read inside the saved bundle — its entry names are the three AC1 names for a Word module build, and the `.docx` entry opens as a zip larger than `MIN_DOCX_BYTES` — and `node tests/plants.mjs` exits 0; the builder PR's smoke workflow is green.
- [x] AC7: `README.md`'s *What the downloads are named* table lists, for each of the eight builds, the bundle name and its three entry names exactly as AC1 captured them, and *What the page shows* and *The scoring file* carry the per-format `#downloadBtn` label the booted page renders, read from the DOM after each format card press.

## Coverage

- AC1 → T1, T2, T6
- AC2 → T1, T6
- AC3 → T2, T6
- AC4 → T6
- AC5 → T3
- AC6 → T4
- AC7 → T5

## Tasks

- [x] T1: Capture the pre-milestone baseline first — the eight builds' questionnaire and `.json` from the deployed page, by the patched `URL.createObjectURL` method (LESSONS M045) — then in `download()` (`index.html:1181-1295`) write `README.txt` into webR's FS, call `zip::zip()` over the three paths with `mode = "cherry-pick"`, read the bundle back and save it once with `saveFile()`; `downloadStem()` unchanged, the REDCap questionnaire written to `<stem>-upload.zip` before zipping.
- [x] T2: Author the `README.txt` text per format as a JS template keyed on `FORMATS[format]`, with the REDCap paragraph only in that format's bundle.
- [x] T3: Delete the handover machinery and `#renameNote` (`index.html:552-593`, `1086-1165`) and the CSS they alone use; rewrite `#descriptorNote` and `#downloadHint` for one file.
- [x] T4: Re-point `tests/smoke.spec.js` A4/A5 at the bundle's entries (read the zip in Node with `DecompressionStream` or `zlib`), add the entry-name assertion, and give `tests/plants.mjs` a plant for each new assertion.
- [x] T5: Rewrite `README.md`'s three sections and its naming table; drop the *two clicks* wording everywhere.
- [x] T6: Verify AC1–AC4 on the served branch against the T1 baseline; record the comparison digests in the work log.

## Work log

- 2026-09-11: created by /milestone-plan.
- 2026-09-11: criteria audit ran in full mode ([O] fresh reader): nine findings across M089/M090, all repaired in the wording — M089 AC2 recast off printed order and date fields (unseeded shuffle; `buildDate`), AC4 names both REDCap bundles, AC6 binds only `plants.mjs` exiting 0, AC7 narrowed to the names and labels the sections document.
- 2026-09-11: plan gate chose a stem-named bundle with the REDCap upload zip renamed `<stem>-upload.zip` over a `-bundle`-suffixed outer name with inner names unchanged because two published names move instead of eight; falsified by a REDCap import refusing the renamed inner archive or visitors reporting confusion between the two zips.
- 2026-09-11: plan gate chose a `README.txt` inside every bundle over page copy alone because the file, not the page, is what reaches a colleague; falsified by the README being reported as noise in the downloads.
- 2026-09-11: plan chose zipping page-side through the package's `{zip}` under webR over a JavaScript zip writer because it adds no dependency and reuses the call D-035 already proved under Emscripten; falsified by `{zip}` ceasing to build there.
- 2026-09-11: plan chose an outer bundle over placing the `.json` inside the REDCap upload archive because that changes a package artifact under D-016's manifest lock and REDCap's tolerance of extra entries is undocumented; falsified by REDCap documenting that extra entries are ignored.
- 2026-09-11: plan gate chose bundle first, flow second (M090 depends on M089) over the reverse or one combined milestone because the bundle deletes most of the download step's copy before the flow re-cuts it, and the combined form would carry ~13 criteria.
- 2026-09-11: implement started; branches `m089-builder-one-file-bundle` (here) and `m089-one-file-bundle` (hitop-builder). Question gate skipped: the plan left no implementation choice open.
- 2026-09-11: T1 baseline captured from the deployed page by a Playwright script (`capture.mjs`, scratch) rather than the browser pane, which blocks fetches to localhost: sixteen files, eight pairs. T1-T3 landed in one builder commit because the page cannot boot with T1's `download()` while T3's handover wiring still names deleted functions. `download()` writes the three files under their bundle names into `/tmp/bundle`, zips them with `zip::zip(mode = "cherry-pick")`, saves `<stem>.zip` once; `saveFile()` loses its `origin` argument; `FORMATS[].mime` goes (unused). Minor amendment: the three `#downloadBtn` labels now end `(.zip bundle)` since the saved file is a zip, and `crosswalkSentence()` no longer names a second button. First comparison of the eight bundles against the baseline: all OK on every AC2 term.
- 2026-09-11: T5 README.md rewritten: intro, *How it works* (the zip step), *What the page shows* step 3 with the three button labels read from the DOM, *The scoring file* now quoting the captured REDCap-module `README.txt` verbatim (diffed against the bundle entry: identical), *What the downloads are named* with a four-column table (bundle, questionnaire, scoring file) verified against the captured entry lists, the shuffle section's second-button sentence, and the layout table's plant count (eight).
- 2026-09-11: T4 `smoke.spec.js` enumerates A1-A7: A4 the bundle's entry names, A5/A6 the `.docx` entry's zip magic and length, A7 the button (was A6); a central-directory reader with `zlib.inflateRawSync` reads the bundle. `plants.mjs` gained (g) README dropped from the zip call and (h) the form stubbed to 12 bytes before zipping; plant (d) re-targeted at the bundle save. Local smoke run green (14 s); `node tests/plants.mjs` exit 0, every plant red on a named assertion, every assertion covered (a: A1; b: A7; c: A1; d: A4, A5, A6; g: A4; h: A5, A6; e: A2; f: A3).
- 2026-09-11: T6 on the served branch (commit 87cb2e4's `index.html`) against the T1 baseline. AC1: eight bundles, each exactly the three expected entries. AC2 all OK: `.txt` sha bc73b984bccd and 13d122901fcd both sides; inner `instrument.csv` sha db794565f74b and 5a2e1c16ec8a both sides; `.json` equal on every field but the excluded ones; `.docx` header and item rows equal (405 and 5 rows), shuffled builds equal on item set and crosswalk presence (false, true). AC3: `without extracting` in the two REDCap READMEs only; `read_module()` and `the file to field` in all eight. AC4: `unzip` and `ditto -x -k` both leave `-upload.zip` on disk, entry list `instrument.csv`, sha 9ebf972768e1 and eb65798f2017 equal to the inner zips the page built. Bundle digests: word 6f3daabf66d3, word-shuffled 1c7b73d26a0e, word-module 6eea9b0c09b1, word-module-shuffled a02c7d35c120, qualtrics 21b495403576, qualtrics-module cd2c21e53cfe, redcap 54b26b22a363, redcap-module f9275075f985. Scripts `capture.mjs` and `compare.py` in the session scratchpad, not committed.
- 2026-09-11: claim audit: 38 claims read, 1 corrected — README.md (the Word/Qualtrics README-difference sentence now names the entry-name line as well as the paragraph; builder commit 61803b4). The reader re-ran `bundleReadme()` and found the quoted REDCap `README.txt` byte-identical.
- 2026-09-11: all tasks checked; builder smoke and plant matrix green locally; no R code changed here, so the r-package `verify` slot has nothing to run. Status → review.
- 2026-09-11: review step 3 checkpoint: AC1-AC5 and AC7 verified with fresh evidence on builder commit 61803b4 and ticked. AC6 waits on the plant matrix (running) and the PR smoke workflow (step 8). cairn_validate green, document() no diff, pkgdown clean, R CMD check running, three reviewers spawned.

## Decisions

## Review

Evidence gathered 2026-09-11 against builder commit 61803b4 (`m089-one-file-bundle`), served locally from the checkout by `tests/serve.mjs` and driven headless by a Playwright script (`capture.mjs`, session scratchpad, not committed). The T1 baseline (sixteen files from the deployed pre-milestone page) reused from the implement session's scratchpad.

- AC1: eight builds driven with the README table's inputs. Each fired exactly one browser download event (counted over a two-second window after the page returned to Ready), named `<stem>.zip`. Every captured bundle's entry list is exactly `[questionnaire, <stem>.json, README.txt]` with the questionnaire `<stem>.docx`, `<stem>.txt`, or `<stem>-upload.zip` for REDCap (`compare.py` "entries OK" on all eight. Entry lists read again with `unzip -Z1`). Bundle digests this pass: word ee3f8e85a849, word-shuffled 01a27ee0001e, word-module b950fdeb34b8, word-module-shuffled 38f05626c4bc, qualtrics d04b731236a3, qualtrics-module b4017fece4d4, redcap 197181e3b349, redcap-module 045fe98fa27b.
- AC2: `compare.py baseline after` exit 0, ALL OK: `.txt` entries byte-identical (sha bc73b984bccd, 13d122901fcd both sides). `.json` entries equal on every field but `buildDate` and, for the two shuffled builds, `itemOrder`, with the same field sets. `.docx` header equal on all four, item rows equal on the unshuffled two (405 and 5 rows), item set equal and crosswalk presence equal (false, true) on the shuffled two. REDCap inner `instrument.csv` byte-identical (sha db794565f74b, 5a2e1c16ec8a).
- AC3: `README.txt` read out of each of the eight bundles: the block headed by the questionnaire entry's name contains "the file to field" in all eight. The `.json` block contains "Keep it with the responses you collect" and "read_module()" in all eight. "Upload this zip file to REDCap as it is, without extracting it" (whitespace-normalized, the sentence wraps) present in the two REDCap bundles and absent from the other six.
- AC4: both REDCap bundles extracted with `unzip -q -o` and with `ditto -x -k`. Each leaves `<stem>-upload.zip` on disk, entry list `instrument.csv`, sha 23f4a999302d (redcap) and ff5b3e1ccf6b (redcap-module) under both tools, equal to the inner zip read straight out of the bundle.
- AC5: DOM query on the booted page: `#saveDescriptor`, `#handover`, `#handoverLive`, `#renameNote` each return null. `.downloadrow button` ids are exactly `["downloadBtn"]`.
- AC7: README.md's naming table, read from the branch, lists the eight bundle names and their questionnaire and scoring-file entries exactly as the captured entry lists above. The third entry `README.txt` is stated in the sentence under the table. `#downloadBtn` labels read from the DOM after each format card press: "Download the Word form (.zip bundle)", "Download the Qualtrics file (.zip bundle)", "Download the REDCap dictionary (.zip bundle)". README.md's *What the page shows* carries all three verbatim (lines 89–91) and *The scoring file* names the bundle by the notice text.
- AC6 (local half): `npx playwright test tests/smoke.spec.js` 1 passed (10.2 s) on the branch, its A4 asserting the three `hitopsr-word-module` entry names and A5/A6 the `.docx` entry's zip magic and length over `MIN_DOCX_BYTES`. `node tests/plants.mjs` exit 0: the unplanted copy passed, all eight plants red on named assertions (a: A1, b: A7, c: A1, d: A4 A5 A6, g: A4, h: A5 A6, e: A2, f: A3). The PR smoke workflow half is read at step 8 once the builder PR exists, the box ticked then.
- Observation, not a defect: one console 404 at boot is Chromium's automatic `/favicon.ico` request (index.html declares no favicon on main either).

Consistency gate 2026-09-11: `cairn_validate` exit 0 (24 advisory warnings, all pre-existing dangling-id and staleness notes). `devtools::document()` no diff. `pkgdown::check_pkgdown()` no problems. `devtools::check()` 0 errors, 0 warnings, 0 notes (4 m 14 s). NEWS.md: no entry due, the package has no code change (the user-visible change is the builder page, whose README carries it). No principle changed, `cairn_impact` skipped.

Independent review 2026-09-11, three fresh-context lenses. Blame-history lens ([S]): zero regressions, every removal traced to a commit that marked the code provisional or to a milestone task. Prior-review lens ([S]): zero findings, both PR-comment probes empty, the archived reviews of M045, M062, M063, M074 and M076 all still honored. Diff-bug lens ([O]) ranked thirteen findings, dispositions below.

- F1 (index.html:1218): ticking a scale mid-build re-enables `#downloadBtn` through `refreshTally()`, and a second `download()` wipes the shared `/tmp/bundle` the first build reads from. Verified against the code. Pre-existing, the old code shared `/tmp/module.*` the same way. Disposition: follow-up candidate row (an in-flight guard, search-first found no row).
- F2 (index.html:541): `#downloadHint` says the button switches off while a build runs, which F1's path falsifies. Same root cause, the old hint made the same claim. Disposition: folded into F1's candidate row.
- F3 (README.md:180): "differ only in the questionnaire entry" understates, the stem line and the `.json` name line differ too. Verified by generating the three READMEs. Disposition: fix now.
- F4 (smoke.spec.js:37): `zipEntries` guards only a missing end-of-central-directory record, a corrupt offset or deflate stream throws and lands in the plant matrix's red-but-silent bucket. Latent, plant (d)'s 12-byte blob short-circuits. Disposition: follow-up candidate row with F5 and F6 (test-reader hardening).
- F5 (smoke.spec.js:64): a compression method other than 8 is returned as stored bytes. Latent, {zip} emits stored and deflate only. Disposition: with F4.
- F6 (smoke.spec.js:166): A4 asserts entry order while its message says membership. Disposition: with F4.
- F7 (index.html:1138): the status line still says "Building the DOCX file…" while the save is a `.zip`. Disposition: reject, the wording is the build's, and M090 re-cuts the download step's copy.
- F8 (index.html:1240): the `>` log line prints basenames where the call passes full paths. Disposition: reject, deliberate, the basenames are what lands in the archive.
- F9 (index.html:536): the REDCap sentence in `#descriptorNote` shows to every format. Disposition: reject, the notice is format-independent by design and M090 re-cuts it.
- F10 (index.html:1076): `wrapIndented` mishandles double spaces and over-long words. Latent, no current input has either. Disposition: reject.
- F11 (index.html:1245): the page relies on {zip} reaching webR as hitop's Import with no presence check. Disposition: reject, the coupling is the one D-035 and D-050 record and `MIN_HITOP` gates it.
- F12 (README.md:205): AC7 asks the table to list "its three entry names" and the table carries two, `README.txt` sits in the sentence under it. Disposition: fix now, a fourth column, then AC7 re-read.
- F13 (index.html:1156): `/tmp/<stem>.zip` bundles are never unlinked. Disposition: reject, at most eight small files in an in-memory FS for the session.
