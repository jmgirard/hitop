# M093: A feasibility evaluation of an online response-collection form as an alternative to Qualtrics and REDCap

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP4
- **Resolves:** —
- **Surface tier:** internal — a synthesis note under `cairn/references/` and a ROADMAP or DECISIONS disposition; no exported surface changes
- **Branch/PR:** —

## Goal

Write down what it would take, and cost, to offer an online form that collects responses to the HiTOP instruments in place of Qualtrics or REDCap, and record Jeff's decision on whether and how to build one.

## Scope

**In:** one synthesis note, `cairn/references/online-collection.md`, comparing four architectures on seven dimensions, each costed under two operators, with a Sources table behind every figure, a per-component build breakdown, and a recommendation; Jeff's disposition at the review gate.

Architectures: **A** a static page that renders the form and saves responses to the respondent's own device, nothing transmitted; **B** the same static page posting each submission to storage the researcher owns (their institution's REDCap through its API, a Supabase or Firebase project, a Google Apps Script endpoint); **C** a hosted service the operator runs, with researcher accounts, a database, and CSV export; **D** a self-hostable kit (a container image or Shiny/plumber app) an institution deploys.

Dimensions: who holds the data; transport and at-rest protection; HIPAA and GDPR obligations and who bears them; the IRB story; hosting cost per month at three scales (a pilot, 1,000 responses a year, 10,000 responses a year); build effort in working sessions; ongoing operations burden (accounts, backups, patching, breach duties).

Operators: Jeff as a solo maintainer; the HiTOP Society or a university unit.

**Out:** building any architecture → planned after the disposition, from the candidate row AC5 leaves (Depends on: M093). A public pkgdown article on collecting responses online → declined at the 2026-09-20 plan gate; revisit once a decision lands. Hosting open PID-5 datasets (issue #87) → untouched here; triaged at `/milestone` §3. The Qualtrics import-format fragility (DESIGN Known issue #5) → unchanged.

## Acceptance criteria

- [ ] AC1: `cairn/references/online-collection.md` exists and its comparison table has one row per architecture named in Scope (A, B, C, D) and one column per dimension named in Scope (seven), with no empty cell; each cell of the HIPAA/GDPR, hosting-cost and operations columns states the solo-maintainer and the Society-operator cases separately.
- [ ] AC2: The note has a Sources table with one row per figure it uses, each row holding the vendor page URL and the date read (YYYY-MM-DD); every cost cell of the comparison table (twelve) and every vendor named in its HIPAA/GDPR column cites a Sources row by number, and every cited row exists with both fields. Statements of law carry no vendor citation.
- [ ] AC3: The note's build-breakdown section holds, for each of B, C and D, five rows (form rendering, submission transport, storage, researcher retrieval, scoring hand-off), and for A the two that apply (form rendering, scoring hand-off), each with an effort estimate in working sessions and a sentence naming where in the package or the builder page the component would live.
- [ ] AC4: The note's recommendation section names one architecture or "none", and for every architecture not recommended (all four when "none") states the condition under which it would become the better choice.
- [ ] AC5: Jeff's disposition at the review gate is recorded in the ROADMAP: a candidate row for the chosen architecture's build, or a candidate row naming the condition for revisiting; a rejection of building any is a `DECISIONS.md` entry with its rationale.
- [ ] AC6: `cairn/references/INDEX.md` carries one line for the note.

## Coverage

- AC1 → T1, T3, T5
- AC2 → T2
- AC3 → T4
- AC4 → T5
- AC5 → T5
- AC6 → T5

## Tasks

- [ ] T1: Requirements section, from the package's own surfaces: a participant link and per-study configuration; response storage; export in the package's item-column names (D-052, D-055; the `rename_*` helpers); module support through descriptors (`read_module()`, the builder's bundle); instrument coverage including the HSUM's display logic (DESIGN Known issue #5 context); the builder's current no-backend posture (its README).
- [ ] T2: Sources table: read and record, with URL and date, GitHub Pages; Fly.io, Render, Railway, and one VPS tier; AWS, GCP and Azure BAA availability and HIPAA-eligible tiers; Supabase and Firebase free and paid tiers; one managed Postgres tier; REDCap API import prerequisites; Qualtrics and REDCap institutional pricing as the baseline where public.
- [ ] T3: Compliance section per architecture and operator: data controller and processor roles; when HIPAA applies and a BAA is needed; GDPR (DPA, EU residency); the data-security questions IRBs ask; consent capture and participant identifiers.
- [ ] T4: Build breakdown (AC3) and architecture fit: rendering from `*_items` and the `*_instructions` objects under IP1; the builder's webR stack against plain JavaScript over a JSON item export (M045 lessons: no shell-out under Wasm, Pages suffices); GP4 dependency posture; D-016 versioning of a served form; D-052/D-055 column names at retrieval; scoring hand-off via descriptors.
- [ ] T5: Comparison table, recommendation with conditions, INDEX line; present the recommendation at the review gate and record the disposition (AC5).

## Work log

- 2026-09-20: created by /milestone-plan. Collision sweep: no ROADMAP row, archive entry or D-entry covers online response collection; the builder README's no-backend posture (M045 lineage) is a design fact, not a rejection. Inbox: one open issue (#87) with no overlap, zero open PRs.
- 2026-09-20: criteria audit ran in reduced mode (internal tier) by a fresh Sonnet reader; returned three fixes (AC2's `$`-grep was a proxy for a cost figure → a Sources table cited by cell; AC4 vacuous under "none" → conditions for every non-recommended architecture; AC5 had no home for "defer" → a candidate row naming the revisit condition) and two asks, both settled by the AC2 rewrite (table cells cite a row; statements of law need no vendor citation).
- 2026-09-20: plan gate chose a feasibility note with no code over a note plus a throwaway front-end prototype and over planning the option-B build directly, because the cost is dominated by storage and compliance, not rendering; falsified by a build estimate the note cannot make without rendering evidence.
- 2026-09-20: plan gate chose comparing all four architectures over B and D only, and over B only, because no prior record rejects any and the note should record why the losers lose; falsified by the C and D sections adding nothing the recommendation reads.
- 2026-09-20: plan gate chose costing each architecture under two operators over one, because C's viability turns on who holds the data; falsified by both operator cases agreeing in every cell.
- 2026-09-20: plan gate chose `cairn/references/` over a public pkgdown article because the note informs a decision not yet made; falsified by a researcher asking why no online form exists.

## Decisions

## Review
