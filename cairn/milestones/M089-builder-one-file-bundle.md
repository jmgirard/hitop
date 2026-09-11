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

- [ ] AC1: For each of the eight builds the README's naming table enumerates (format × whole-or-selection × shuffle), driven on the page with the same inputs, the browser is asked to save exactly one file, named `<stem>.zip`, whose entry list read out of the captured bundle is exactly three names: the questionnaire (`<stem>.docx`, `<stem>.txt`, or for REDCap `<stem>-upload.zip`), `<stem>.json`, and `README.txt`.
- [ ] AC2: For each of those eight builds, the questionnaire and `.json` entries inside the bundle carry what the same inputs produced on the deployed pre-milestone page, captured in the same session: the `.txt` entries byte-identical; the `.json` entries identical on every field but `buildDate` and, for shuffled builds, `itemOrder`; the `.docx` entries identical on the parsed header and, for unshuffled builds, the parsed item rows, and for shuffled builds on the item set and on whether a crosswalk table is present; the REDCap inner zip's `instrument.csv` byte-identical.
- [ ] AC3: `README.txt` read out of each of the eight captured bundles names the questionnaire entry as the file to field, says the `.json` entry is kept with the collected responses and read by `read_module()`, and — in the two REDCap bundles and no other — says the inner `-upload.zip` is uploaded to REDCap without being extracted.
- [ ] AC4: Extracting each of the two captured REDCap bundles with `unzip` and with macOS `ditto -x -k` leaves `<stem>-upload.zip` on disk as a zip whose entry list is unchanged from the inner zip the page built.
- [ ] AC5: A DOM query of the booted page for `#saveDescriptor`, `#handover`, `#handoverLive` and `#renameNote` returns no element, and the download step's `.downloadrow` holds exactly one button, `#downloadBtn`.
- [ ] AC6: `tests/smoke.spec.js`'s download assertions read inside the saved bundle — its entry names are the three AC1 names for a Word module build, and the `.docx` entry opens as a zip larger than `MIN_DOCX_BYTES` — and `node tests/plants.mjs` exits 0; the builder PR's smoke workflow is green.
- [ ] AC7: `README.md`'s *What the downloads are named* table lists, for each of the eight builds, the bundle name and its three entry names exactly as AC1 captured them, and *What the page shows* and *The scoring file* carry the per-format `#downloadBtn` label the booted page renders, read from the DOM after each format card press.

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

## Decisions

## Review
