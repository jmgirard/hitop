# M093: A feasibility evaluation of an online response-collection form as an alternative to Qualtrics and REDCap

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP4
- **Resolves:** —
- **Surface tier:** internal — a synthesis note under `cairn/references/` and a ROADMAP or DECISIONS disposition; no exported surface changes
- **Branch/PR:** `m093-online-collection-feasibility` — https://github.com/jmgirard/hitop/pull/101

## Goal

Write down what it would take, and cost, to offer an online form that collects responses to the HiTOP instruments in place of Qualtrics or REDCap, and record Jeff's decision on whether and how to build one.

## Scope

**In:** one synthesis note, `cairn/references/online-collection.md`, comparing four architectures on seven dimensions, each costed under two operators, with a Sources table behind every figure, a per-component build breakdown, and a recommendation; Jeff's disposition at the review gate.

Architectures: **A** a static page that renders the form and saves responses to the respondent's own device, nothing transmitted; **B** the same static page posting each submission to storage the researcher owns (their institution's REDCap through its API, a Supabase or Firebase project, a Google Apps Script endpoint); **C** a hosted service the operator runs, with researcher accounts, a database, and CSV export; **D** a self-hostable kit (a container image or Shiny/plumber app) an institution deploys.

Dimensions: who holds the data; transport and at-rest protection; HIPAA and GDPR obligations and who bears them; the IRB story; hosting cost per month at three scales (a pilot, 1,000 responses a year, 10,000 responses a year); build effort in working sessions; ongoing operations burden (accounts, backups, patching, breach duties).

Operators: Jeff as a solo maintainer; the HiTOP Society or a university unit.

**Out:** building any architecture → planned after the disposition, from the candidate row AC5 leaves (Depends on: M093). A public pkgdown article on collecting responses online → declined at the 2026-09-20 plan gate; revisit once a decision lands. Hosting open PID-5 datasets (issue #87) → untouched here; triaged at `/milestone` §3. The Qualtrics import-format fragility (DESIGN Known issue #5) → unchanged.

## Acceptance criteria

- [x] AC1: `cairn/references/online-collection.md` exists and its comparison table has one row per architecture named in Scope (A, B, C, D) and one column per dimension named in Scope (seven), with no empty cell; each cell of the HIPAA/GDPR, hosting-cost and operations columns states the solo-maintainer and the Society-operator cases separately.
- [x] AC2: The note has a Sources table with one row per figure it uses, each row holding the vendor page URL and the date read (YYYY-MM-DD); every cost cell of the comparison table (twelve) and every vendor named in its HIPAA/GDPR column cites a Sources row by number, and every cited row exists with both fields. Statements of law carry no vendor citation.
- [x] AC3: The note's build-breakdown section holds, for each of B, C and D, five rows (form rendering, submission transport, storage, researcher retrieval, scoring hand-off), and for A the two that apply (form rendering, scoring hand-off), each with an effort estimate in working sessions and a sentence naming where in the package or the builder page the component would live.
- [x] AC4: The note's recommendation section names one architecture or "none", and for every architecture not recommended (all four when "none") states the condition under which it would become the better choice.
- [x] AC5: Jeff's disposition at the review gate is recorded in the ROADMAP: a candidate row for the chosen architecture's build, or a candidate row naming the condition for revisiting; a rejection of building any is a `DECISIONS.md` entry with its rationale.
- [x] AC6: `cairn/references/INDEX.md` carries one line for the note.

## Coverage

- AC1 → T1, T3, T5
- AC2 → T2
- AC3 → T4
- AC4 → T5
- AC5 → T5
- AC6 → T5

## Tasks

- [x] T1: Requirements section, from the package's own surfaces: a participant link and per-study configuration; response storage; export in the package's item-column names (D-052, D-055; the `rename_*` helpers); module support through descriptors (`read_module()`, the builder's bundle); instrument coverage including the HSUM's display logic (DESIGN Known issue #5 context); the builder's current no-backend posture (its README).
- [x] T2: Sources table: read and record, with URL and date, GitHub Pages; Fly.io, Render, Railway, and one VPS tier; AWS, GCP and Azure BAA availability and HIPAA-eligible tiers; Supabase and Firebase free and paid tiers; one managed Postgres tier; REDCap API import prerequisites; Qualtrics and REDCap institutional pricing as the baseline where public.
- [x] T3: Compliance section per architecture and operator: data controller and processor roles; when HIPAA applies and a BAA is needed; GDPR (DPA, EU residency); the data-security questions IRBs ask; consent capture and participant identifiers.
- [x] T4: Build breakdown (AC3) and architecture fit: rendering from `*_items` and the `*_instructions` objects under IP1; the builder's webR stack against plain JavaScript over a JSON item export (M045 lessons: no shell-out under Wasm, Pages suffices); GP4 dependency posture; D-016 versioning of a served form; D-052/D-055 column names at retrieval; scoring hand-off via descriptors.
- [x] T5: Comparison table, recommendation with conditions, INDEX line; present the recommendation at the review gate and record the disposition (AC5). The gate half stays open for /milestone-review.

## Work log

- 2026-09-20: created by /milestone-plan. Collision sweep: no ROADMAP row, archive entry or D-entry covers online response collection; the builder README's no-backend posture (M045 lineage) is a design fact, not a rejection. Inbox: one open issue (#87) with no overlap, zero open PRs.
- 2026-09-20: criteria audit ran in reduced mode (internal tier) by a fresh Sonnet reader; returned three fixes (AC2's `$`-grep was a proxy for a cost figure → a Sources table cited by cell; AC4 vacuous under "none" → conditions for every non-recommended architecture; AC5 had no home for "defer" → a candidate row naming the revisit condition) and two asks, both settled by the AC2 rewrite (table cells cite a row; statements of law need no vendor citation).
- 2026-09-20: plan gate chose a feasibility note with no code over a note plus a throwaway front-end prototype and over planning the option-B build directly, because the cost is dominated by storage and compliance, not rendering; falsified by a build estimate the note cannot make without rendering evidence.
- 2026-09-20: plan gate chose comparing all four architectures over B and D only, and over B only, because no prior record rejects any and the note should record why the losers lose; falsified by the C and D sections adding nothing the recommendation reads.
- 2026-09-20: plan gate chose costing each architecture under two operators over one, because C's viability turns on who holds the data; falsified by both operator cases agreeing in every cell.
- 2026-09-20: plan gate chose `cairn/references/` over a public pkgdown article because the note informs a decision not yet made; falsified by a researcher asking why no online form exists.
- 2026-09-20: /milestone-implement started; branch `m093-online-collection-feasibility` cut from pushed main at `6d2be446`. Question gate skipped: the plan fixed the architectures, dimensions, operators and the note's home, and the remaining tier picks are routine. The simple-english lint hook reports pre-existing violations in ROADMAP and this file on every edit; both are append-only history and are left as they are.
- 2026-09-20: T1 done — note skeleton (Provenance, Scope, Evidence snapshot) and a Requirements section of eight rows (R1–R8), each naming the package surface it derives from: descriptor format and `itemOrder` (`R/module_file.R`), `item_names()` (`R/util.R:697`), the four `*_instructions` objects, the HSUM's 638 gated fields over nine choice sets, IP1/D-016 for a served form, GP4 for the dependency posture. Vendor-page reads (T2) delegated to one [S] subagent, running.
- 2026-09-20: T2 done — the [S] subagent returned 21 records, 8 of them resting on secondary sources because the page rendered its figures by script or refused the fetch; those 8 were re-read in the built-in browser (Render, Google Cloud's covered-products list, Google Workspace HIPAA, both HHS pages, GDPR Art. 4 and 28, a university REDCap API page, Fly.io machines, Supabase regions). Hetzner's price list and Fly's managed-Postgres page did not load, so the VPS row is DigitalOcean and the managed-Postgres rows are Neon and Render, all read directly; the note's Sources section names the three pages tried and not read. The Sources table splits vendor rows (S1–S17) from regulator and legal-text rows (L1–L4) so that statements of law cite no vendor.
- 2026-09-20: T3 done — compliance section with four tables: controller/processor per architecture, business associate per architecture and operator, the IRB story, and consent and identifiers. Written before T2 landed and re-cited after; one deviation logged: the section was appended through a shell heredoc rather than the Edit tool, and every later edit used Edit.
- 2026-09-20: T4 done — architecture-fit section (IP1 rendering from the tables, the builder's webR stack rejected for a participant-facing page, GP4, D-016 build dates on a served form, D-052/D-055 names, descriptor hand-off) and a build breakdown of A 6, B 11, C 14, D 12 sessions plus one per release. One finding the plan did not anticipate: a static page cannot hold a REDCap API token, so the REDCap variant of B needs a relay, which is C or D.
- 2026-09-20: T5 done — comparison table (4 rows × 7 columns, no empty cell, solo and institution stated in the HIPAA/GDPR, cost and operations columns) with a 12-cell hosting-cost table beneath it, each cell citing a Sources row; recommendation B in two steps (A's renderer first), with the condition under which A, C, D and "none" each become better; INDEX line. AC1–AC4 and AC6 checked by a script in the session (7 columns, 4 rows, 12 cited cost cells, 21 source rows each with URL and date, five and two build rows, four conditions, one INDEX line). `cairn_validate` passes with 24 pre-existing advisories. Checkpoint: T2–T5 land in one commit rather than four, the note being one file grown in place.
- 2026-09-20: claim audit: not owed — internal tier.
- 2026-09-20: verify slot: no R code, roxygen or data changed, so `devtools::test()` and `document()` were not run; the branch diff is three files under `cairn/`. Status set to review.
- 2026-09-20: /milestone-review started; AC1–AC4 and AC6 verified by script and ticked, three AC3 cells expanded to name a location; `devtools::check()` and the [O] reviewer still running at this checkpoint, AC5 open for the gate.
- 2026-09-20: gate held: disposition = candidate row for B's build; F13 rejected; 13 fix-now findings applied; `devtools::check()` 0/0/0.
- 2026-09-20: step-7 approval: m093-online-collection-feasibility approved for merge
- 2026-09-20: the AC5 row pushed ROADMAP to 60 lines / 24,774 bytes, over both caps; `cairn_validate` failed after the push that opened PR #101. Remedy on the branch: the superseded confidence-interval candidate row absorbed into the norms-schema row that superseded it, the new row compressed, the hygiene stamp shortened; ROADMAP now 59 lines and under 24,000 bytes, validate green.
- 2026-09-20: PR #101 opened, marker written. CI wait timed out at the harness ceiling; fresh `gh pr checks`: line endings, pkgdown, test-coverage pass; macos-latest (release) failed in setup-r-dependencies (pak: flextable_0.10.1.tgz "unknown archive type", a corrupt runner download, not the package); the four remaining check jobs pending. Resume: rerun the failed macOS job once the run completes, wait for green, merge.
- 2026-09-20 resume: PR #101 OPEN; main unmoved at `6d2be446`. conversation: PR #101 — empty read (no reviews, no comments, no unresolved threads). Fresh `gh pr checks`: six pass, macOS fail (runner download), Windows pending.
- 2026-09-20: step-7 approval: m093-online-collection-feasibility approved for merge (re-posed on resume)
- 2026-09-20: blocked: PR #101's macos-latest (release) check fails on re-run too — CRAN serves the R 4.6 macOS binary `flextable_0.10.1.tgz` as a zstd archive (magic bytes 28 b5 2f fd) and pak 0.11.1 / pkgdepends has no zstd extraction, so `setup-r-dependencies` aborts with "unknown archive type"; no upstream issue or fix found. Seven other checks green. Jeff chose to park rather than edit the workflow on this branch. Resume with `/milestone-review M093` once pak or CRAN fixes it; the approval and PR stand.
- 2026-09-20 18:06Z: /milestone-review re-invoked on `blocked` (user override, logged). Re-check: main unmoved, PR #101 open, 7/8 checks green, macOS red; CRAN binary still zstd (28 b5 2f fd), pak latest still v0.11.1 (2026-07-22), no new pak issue. Blocker unchanged; still parked.
- 2026-09-20: Jeff chose the CI workaround on this branch (supersedes the parking decision): `.github/workflows/R-CMD-check.yaml` pins the macOS matrix entry to R 4.5, whose CRAN macOS binaries are still gzip (probed `big-sur-arm64/contrib/4.5/flextable_0.10.1.tgz`, magic 1f 8b), with a comment naming the revert condition. No branch protection names the check, so the job's new name is safe. Status blocked → in-progress → review in this commit; the diff now touches executable surface, so the three-lens review fan-out runs before the re-posed gate.

## Decisions

## Review

- 2026-09-20 sync: branch contains `origin/main` tip `6d2be446`; no PR exists; owner mode. Diff against main: four files under `cairn/`, no R code, so the profile's `verify` slot has nothing to re-run.
- AC1 evidence: script over the comparison table — 7 dimension columns in Scope order, 4 rows (A–D), 0 empty cells; the HIPAA/GDPR, hosting-cost and operations cells of every row each state a solo case and an institution case. Pass.
- AC2 evidence: script — Sources table 21 rows (S1–S17, L1–L4), every row with an `http` URL and a `YYYY-MM-DD` date; the 12 cells of the hosting-cost table each cite an S row; the HIPAA/GDPR column cites S9, S7, S16 (B) and S6, S7, S8 (C), A and D naming no vendor; every cited id exists; the statements of law cite L rows only. Three rows are cited nowhere (S12 REDCap API, S13 Qualtrics pricing, S17 Cloudflare Workers) — surplus, not a criterion failure; sent to triage. Pass.
- AC3 evidence: script — build breakdown has 5 rows for B, C and D and 2 for A, each with an integer session count; three cells read only "As in A." / "As in C." and named no location, so review expanded each to name the package or the `hitop-server` repository (a fix-now on the branch, three cells, no figure changed); after the edit every row names where the component lives. Totals re-summed: A 6, B 11, C 14, D 12. Pass.
- AC4 evidence: the recommendation names B; conditions stated for A, C, D and "none". Pass.
- AC6 evidence: `grep -c online-collection.md cairn/references/INDEX.md` = 1. Pass.
- AC5 evidence: at the 2026-09-20 gate Jeff chose the candidate row for B's two-step build; the row is in `ROADMAP.md` Candidates (first row, "Build architecture B …", Depends on M093). Pass.
- Gate triage 2026-09-20: F1–F12 and F16 applied as fix-now in one commit on the branch (the note and INDEX; S17 dropped, so the Sources table is 20 rows, every row cited); F13 rejected — the AC3 clause "in the package or the builder page" reads as examples of a home, every row names one; F14 and F15 rejected with the reasons above. Criterion script re-run after the edits: AC1–AC4 and AC6 still pass.
- Consistency gate: `cairn_validate` exit 0, 23 lines, 0 FAIL, 2 pre-existing WARN (references staleness on `schmukle2026.md`, 23 dangling id tokens); no principle changed, so `cairn_impact` skipped. `devtools::document()` no diff; `pkgdown::check_pkgdown()` no problems; README.Rmd and NEWS untouched (no user-visible change); `devtools::check()` result recorded below.
- `devtools::check()`: 0 errors, 0 warnings, 0 notes (5m 39s). Toolchain gate passes.
- Independent review: internal tier, docs-only diff, one fresh-context [O] reviewer; 16 findings ranked by the reviewer, listed below with the gate's disposition (`fix-now` edits land on the branch before the push).
  - F1 (`online-collection.md:34,105`): the scoring call `score_hitopsr(data, items = read_module(f), layout = "printed")` is wrong — the module goes in `module =` and `items` takes columns; the article's real call is `score_hitopsr(x, items = seq_along(x), module = m, layout = "printed")`. Confirmed against `R/score_hitopsr.R:56`. Disposition: fix-now.
  - F2 (`:42,59,70,85,165,176` against `:107,184`): B still lists institutional REDCap as a store in six places after the note's own finding that a static page cannot hold an API token. Disposition: fix-now, reconcile B's rows to the vendor stores and say REDCap stays the data-dictionary route.
  - F3 (`:16`): `hitop_artifacts` is 39 rows over 24 files, not 20. Confirmed. Disposition: fix-now.
  - F4 (`:165,176` against `:209`): $599 is the Supabase Team plan, the HIPAA add-on's price unshown. Disposition: fix-now, say so in the cells and add the unknown to Open questions.
  - F5 (`:32`): the Qualtrics display-logic claim cites DESIGN Known issue #5; the source sentence is DESIGN's Generators family line. Confirmed. Disposition: fix-now.
  - F6 (`:212,213,217,107`): S12, S13, S17 cited nowhere; the token claim at `:107` is S12's figure, uncited. Disposition: fix-now — cite S12 at `:107`, cite S13 and S14 as the baseline in "What the package offers today", drop S17.
  - F7 (`INDEX.md:10`): line out of alphabetical order. Disposition: fix-now.
  - F8 (`:1`, `INDEX.md:10`): milestone number in the H1 and INDEX description, unlike sibling pages. Disposition: fix-now.
  - F9 (`:232` against `:7`): the Disposition section asserts status. Disposition: fix-now, point at ROADMAP and DECISIONS without stating a state.
  - F10 (`:7` against `:182`): "no architecture is chosen here" beside a recommendation. Disposition: fix-now, "it recommends one and decides nothing".
  - F11 (`:171`): 2 KB a row is low for a Firestore document (5–10 KB); the bound still holds. Disposition: fix-now, state both figures.
  - F12 (`:142`): C's scoring hand-off row names no home. Disposition: fix-now.
  - F13 (`:113–159`): AC3 says "where in the package or the builder page", and most components live in two proposed repositories. Disposition: decided at the gate — an amendment return on AC3's enumeration, or a rejection.
  - F14 (`:164–167`): the comparison table's four cost cells are uncited; the twelve cited cells are the hosting-cost table's. Disposition: reject — AC2's "(twelve)" names the 4×3 hosting-cost table, and the comparison cell says it summarizes it.
  - F15 (`:37,97`): the builder's twenty-second load cannot be checked from this repo. Disposition: reject — an external repository named in Provenance, its two M045 facts confirmed.
  - F16 (`:176`): a yearly write count compared against a daily quota. Disposition: fix-now, compare a day's plausible burst.
- Second review pass (2026-09-20), executable surface: the CI pin in `.github/workflows/R-CMD-check.yaml`, three fresh-context lenses.
  - [O] C1 (`R-CMD-check.yaml:27,31`): the matrix loses release-on-macOS entirely and checks R 4.5.3 twice (oldrel-1 is 4.5.3 today); the comment and log do not say so. Disposition: fix-now — the comment states the lost coverage; a candidate row tracks the revert.
  - [O] C2 (`:27`): a bare `'4.5'` pin has no expiry; CRAN freezes a minor's macOS binary tree once it is oldrel-2, so the job would go stale silently after R 4.7. Disposition: follow-up — the same candidate row, promotion condition stated.
  - [O] C3 (`:25–26`): the revert condition pegs "R 4.6"; pkgcache's dev NEWS already moves R 4.7 macOS binaries to a new path, and no zstd work exists in pak, pkgdepends or pkgcache. Disposition: fix-now — the comment names "the R version `release` resolves to" instead of 4.6.
  - [O] C4 (`:24`): "fails on flextable" reads as if flextable were the fault; it is the first zstd binary pak reaches. Disposition: fix-now, one clause.
  - [O] C5 (informational): pak's own macOS-arm64 build for R 4.5 unproven until CI runs. Disposition: noted — the pending `macos-latest (4.5)` run is the proof.
  - [O] alternative: `use-public-rspm: true` on the macOS entry (P3M serves gzip) untested; `pak-version: devel` would not help (no zstd support anywhere in the dev NEWS); `PKG_PLATFORMS=source` would force the ragg/systemfonts chain to compile without system headers. Disposition: noted in the candidate row as the untested alternative.
  - [S] blame-history: no finding — `r: 'release'` came from the usethis boilerplate at M004 (`d6164cb9`), never revisited; no D-entry governs CI platform coverage; no prior pin/unpin on any workflow.
  - [S] prior-review comments: "no prior-review evidence" — no archived Review finding touches CI config; the PR-comments probe returned an empty list.
