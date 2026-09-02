<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M079: The online exports pad item numbers to the instrument's width, not to the export's own

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP2, GP3   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

`build_qualtrics_txt()` and `build_redcap_zip()` zero-pad item numbers to the
width of the instrument's largest item number, stated by the wrapper that calls
them, rather than to the width of whichever items that one call exports.

## Scope
<!-- owner: plan · create/amend-via-gate -->

Surface tier: **internal** — no external consumer's output changes. Both
builders derive their padding width from the items handed to them
(`R/generate_qualtrics.R:287`, `R/generate_redcap.R:289`), so a HiTOP-SR module
whose items were all below 100 would be exported as `hsr_07`, which
`label_hitopsr()` cannot match. That module cannot be built from today's
keying: measured 2026-09-01 over all 76 rows of `hitopsr_scales`, the smallest
per-scale largest item number is 151 (`difficultiesReachingOrgasm`), against
`max(hitopsr_items$HSR)` of 405, so every module is already three digits wide.
The deliverable is therefore where the two builders get the width and the tests
that hold it there — no shipped or generated file moves.

**In:** a required instrument-width argument on both builders, passed by all ten
`generate_{qualtrics,redcap}_*` wrappers from the instrument's own item table;
the Qualtrics builder's inline width computation replaced by `item_names()`
(the M077 review's finding 5); tests for the width source, the required
argument, and a rebuild of every shipped online export against the committed
copy; the padding comments in `R/util.R:583-586` and both builders; the two
HiTOP-SR online generators' help pages stating that a module export keeps the
full instrument's width.

**Out:** renumbering module exports (excluded deliberately — D-036) · a
changelog entry, no user-visible output having changed (plan gate, 2026-09-01)
· `module` support for the HiTOP-BR and PID-5 generators → the "Generalize
modularization to BR/PID-5" candidate row · a descriptor recording the column
names an export produces → the descriptor-format candidate row · PID-5 dataset
item-column naming → its own candidate row.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: `build_qualtrics_txt()` and `build_redcap_zip()` pad to the
      instrument width they are given, not to the items they are given: called
      directly with an items frame whose largest item number is 45 and an
      instrument width of 405, they write the question id `HSR_007` and the
      variable name `hsr_007` for item 7.
- [ ] AC2: Both builders require that width — a direct call omitting it aborts,
      rather than falling back to the exported items' own largest number.
- [ ] AC3: No shipped online export's item names move: for every row of
      `hitop_artifacts` whose `format` is `qualtrics` or `redcap` — the domain
      enumerated from the manifest — a fresh default build's item variable
      names are identical to those in the committed file the row names.
- [ ] AC4: `Rscript -e 'devtools::test()'` clean and
      `Rscript -e 'devtools::document()'` producing no diff (the profile's
      `verify` slot).

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number. -->

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T4
- AC4 → T1, T2, T3, T4, T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [ ] T1: Give `build_qualtrics_txt()` (`R/generate_qualtrics.R:252`) and
      `build_redcap_zip()` (`R/generate_redcap.R:260`) a required instrument-width
      argument; build the Qualtrics ids through `item_names()` instead of the
      inline `nchar(as.character(max(items[[1]])))` at `R/generate_qualtrics.R:287`;
      pass the width from all ten wrappers, read from the instrument's own item
      table (`max(hitopsr_items$HSR)`, `max(hitopbr_items$HBR)`, the PID-5
      version columns).
- [ ] T2: Test AC1 and AC2 by direct call on both builders with a synthetic
      items frame. Prove the AC1 test discriminates: revert each builder to
      `max(items[[1]])`, record the red run in the work log, restore.
- [ ] T3: End-to-end smoke — a multi-scale HiTOP-SR module through
      `generate_qualtrics_hitopsr()` and `generate_redcap_hitopsr()` names its
      items exactly `item_names(prefix, <the module's numbers>, max_n =
      max(hitopsr_items$HSR))`.
- [ ] T4: Test AC3 — iterate the `qualtrics`/`redcap` rows of
      `hitop_artifacts`, rebuild each with its generator's defaults into a temp
      file, and compare item variable names against the committed
      `inst/extdata/` file. Parse inside the REDCap container (zip builds are
      not byte-deterministic) and read the expected names off the committed
      artifact, never off the fresh build.
- [ ] T5: Correct the padding comments (`R/util.R:583-586` says the width is
      "the export's for the generators") and both builders' comments; add to
      `generate_qualtrics_hitopsr()` and `generate_redcap_hitopsr()` help that a
      module export keeps the full instrument's item-name width; `document()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-09-01: created by /milestone-plan.
- 2026-09-01: criteria audit ran in reduced mode ([O], fresh context; internal tier) and returned a finding on all five drafted criteria — an unreachability floor and a comment-prose promise (both instrument properties), a mutation-discrimination clause (instrument), a merge-base rebuild comparison and a 76-module per-rendering enumeration (both disproportionate to the tier); each had one clear answer and was fixed before writing, leaving four criteria.
- 2026-09-01: plan gate chose an instrument-width argument on the two builders over deriving the width inside `apply_module()` because the five PID-5 wrappers of each family never call `apply_module()` and would keep a second width source; falsified by a wrapper family that cannot name its instrument's item table.
- 2026-09-01: plan gate chose comparing rebuilt exports' item names over comparing whole files because the committed artifacts carry build stamps unrelated to naming; falsified by a naming drift that leaves the item names equal while changing the rest of the file.
- 2026-09-01: plan gate chose fixing the latent defect over closing or narrowing the candidate row (Jeff, at the gate); falsified by the fix proving larger than the ten call sites and three tests scoped here.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
